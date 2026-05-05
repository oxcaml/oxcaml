/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                          Sadiq Jaffer, Opsian                          */
/*                                                                        */
/*   Copyright 2021 Opsian Ltd                                            */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include "caml/runtime_events.h"
#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/osdeps.h"
#include "caml/platform.h"
#include "caml/startup_aux.h"

#include <fcntl.h>
#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <process.h>
#include <processthreadsapi.h>
#include <wtypes.h>
#else
#include <sys/mman.h>
#endif

/* hardware events are linux only for now */
#ifdef PERF_COUNTERS
#include <linux/perf_event.h>
#include <sys/syscall.h>
#include <x86intrin.h>
#endif

#if defined(HAS_UNISTD)
#include <unistd.h>
#endif

#define RUNTIME_EVENTS_VERSION 2

/*
This file contains the implementation for runtime_events's producer. The
consumer can be found in runtime_events_consumer.

Runtime_events is a transport for tracing and counter events from the OCaml
runtime. When enabled the caml_ev_* probes emit events that get written to
per-domain memory-mapped ring buffers. Consumers can be written that use the
OCaml or C apis to consume these events asynchronously. This can be done both
inside or outside the process.

The ring buffer is structured as a flight recorder, overwriting old data when
there is insufficient space to write new events. This enables users to
potentially only read the ring when some anomalous event occurs. No coordination
is needed with consumers who read the events - they detect races with the
producer and discard events when that happens.

The producer code is contained here . By default a <pid>.events file is created
in the current directory (overridable by setting OCAML_RUNTIME_EVENTS_DIR).
This file contains a ring buffer for each possible domain
(caml_params->max_domains). It is laid out in a structure that enables sparsity.
On-disk (or in-memory) footprint is proportional to the max number of
concurrent domains the process has ever run.

On disk structure:

----------------------------------------------------------------
| File header (version, offsets, etc..)                        |
----------------------------------------------------------------
| Ring 0..caml_params->max_domains metadata                     |
| (head and tail indexes, one per cache line)                  |
----------------------------------------------------------------
| Ring 0..caml_params->max_domains data                         |
| (actual ring data, default 2^16 words = 512k bytes)          |
----------------------------------------------------------------
| Custom event IDs                                             |
| 2^13 char[128] = 1M bytes                                    |
----------------------------------------------------------------
*/

typedef enum { EV_RUNTIME, EV_USER } ev_category;

/* These store state for the current ring buffers open for writing */
static struct runtime_events_metadata_header *current_metadata = NULL;
static int current_ring_total_size;
static char_os *runtime_events_path;
static char_os *current_ring_loc = NULL;

#ifdef _WIN32
static HANDLE ring_file_handle;
static HANDLE ring_handle;
#endif

/* This comes from OCAMLRUNPARAMs and is initialised in
   caml_runtime_events_init */
static int ring_size_words;

/* This is set if the OCAML_RUNTIME_EVENTS_PRESERVE environment
  variable is present and determines whether the ring buffer is
  cleaned up on program exit or not. It may be preserved to allow
  tooling to analyse very short running programs where there would
  be a race to read their ring buffers. */
static int preserve_ring = 0;

static atomic_uintnat runtime_events_enabled = 0;
static atomic_uintnat runtime_events_paused = 0;

static atomic_uintnat runtime_custom_event_index = 0;

/* List of globally known events. This is used to figure which event has a
   given string ID. */
static value user_events = Val_none;
static caml_plat_mutex user_events_lock;

/* Custom type write buffer */
static void write_to_ring(ev_category category, ev_message_type type,
                          int event_id, int event_length, uint64_t *content,
                          int word_offset);

static void events_register_write_buffer(int index, value event_name);
static void runtime_events_create_from_stw_single(void);

static void stw_teardown_runtime_events(
  caml_domain_state *domain_state,
  void *remove_file_data, int num_participating,
  caml_domain_state **participating_domains);

#ifdef PERF_COUNTERS
/* Hardware performance-monitoring counters (PMC).

   When configured via the OCAML_RUNTIME_EVENTS_PERF_COUNTERS env var, we
   sample a fixed set of hardware counters on every EV_BEGIN / EV_EXIT
   runtime-phase event and on every user-span Begin / End event, appending
   the values to the event's entry in the ring buffer. The sample reads
   use the x86 userspace RDPMC instruction so we don't pay a syscall per
   event on the hot path.

   The kernel exposes each counter's current state (offset, hardware index,
   width, scheduling seqlock) via a page mmap'd from the perf event fd.
   Reading the counter requires a seqlock dance: snapshot the lock, read
   index/offset/width, execute RDPMC, re-read the lock, retry if the kernel
   re-scheduled the counter in between.

   Lifecycle across a process:

   1. The first caml_runtime_events_start (or the first after a destroy)
      parses the env var into the process-static perf_counter_configs[] /
      num_perf_configs under the existing start/destroy STW. Subsequent
      starts without an intervening destroy see num_perf_configs != 0 and
      skip the parse.
   2. Each OCaml thread that emits events lazily allocates a per-thread
      struct perf_counters via get_thread_counters(). The domain/thread
      lifecycle hooks (caml_runtime_events_domain_start / _thread_start)
      call get_thread_counters() eagerly so the first event doesn't take
      the setup cost. perf_events_setup() opens one perf_event per config
      (first is the pinned group leader), mmaps the metadata page, and
      records the RDPMC offset/index/width via perf_events_setup_rdpmc.
   3. On each sampled event, write_to_ring calls perf_events_sample(),
      which does N userspace RDPMC reads under the leader's seqlock.
   4. On thread/domain stop, cleanup_thread_counters() munmaps each page.
      On caml_runtime_events_destroy, num_perf_configs is cleared and
      perf_counter_generation is bumped so other threads will notice
      their state is stale on their next event.

   Failure model: any setup failure (perf_event_open denied, mmap failed,
   RDPMC unavailable, too many counters for the hardware) flips
   perf_counters_globally_disabled for the remainder of the process. All
   subsequent get_thread_counters() calls short-circuit and return NULL,
   so sampling is skipped and callbacks see empty arrays. This is the
   single graceful-degradation path; there is no retry.
*/

/* Process-static config. Populated once by parse_perf_config_string under
   the start/destroy STW, cleared by destroy. Read without atomics from the
   event-emit paths, which can't overlap with start/destroy. */
static uint64_t perf_counter_configs[RUNTIME_EVENTS_MAX_PERF_EVENTS];
static int num_perf_configs = 0;

/* Cached offset/index/width from the kernel's perf_event_mmap_page, read
   on every sample to avoid re-parsing the page. */
struct perf_counter_rdpmc_info {
  uint64_t offset;
  uint32_t index;
  uint16_t width;
};

/* Per-thread PMC state. Owned exclusively by the thread that allocated it. */
struct perf_counters {
  int ncounters;
  void* mmap_pages[RUNTIME_EVENTS_MAX_PERF_EVENTS];
  /* All counters in a group share a schedule, so the leader's seqlock is
     sufficient to detect re-scheduling of any counter in the group. */
  uint32_t* leader_seq_lock;
  uint32_t last_seq;
  struct perf_counter_rdpmc_info rdpmc_info[RUNTIME_EVENTS_MAX_PERF_EVENTS];
};

/* Bumped by destroy. Threads see the mismatch on their next call to
   get_thread_counters and tear down their state so stale mmap_pages don't
   survive a destroy+start cycle. */
static _Atomic uint64_t perf_counter_generation = 0;

static CAMLthread_local struct perf_counters* thread_counters = NULL;
static CAMLthread_local uint64_t thread_counter_generation = 0;

/* Circuit breaker: flipped on the first setup failure seen by any thread,
   reset only on destroy. All subsequent get_thread_counters calls return
   NULL until the next destroy+start. */
static atomic_bool perf_counters_globally_disabled = false;

/* Compiler-only memory barrier: sufficient on x86 where loads/stores to
   the kernel's mmap page are not reordered by hardware. */
#define seqlock_barrier() asm volatile("" ::: "memory")

/* Parse a comma-separated list of hex raw perf event codes each prefixed
   with 'r' (e.g. "r3c,r3d") into perf_counter_configs[]. Returns NULL on
   success, or a pointer to a static error string on failure. */
static const char* parse_perf_config_string(const char* format)
{
  static char err[256];
  int count = 0;
  while (*format != '\0') {
    if (format[0] != 'r') {
      snprintf(err, sizeof err,
               "events '%s': cannot parse (only 'rNNN' events supported)",
               format);
      return err;
    }
    format++;
    char* endptr;
    unsigned long config = strtoul(format, &endptr, 16);
    if (*endptr == ',') format = endptr + 1;
    else if (*endptr == '\0') format = endptr;
    else {
      snprintf(err, sizeof err,
               "events '%s': cannot parse (only 'rNNN' events supported)",
               format);
      return err;
    }
    if (count >= RUNTIME_EVENTS_MAX_PERF_EVENTS) {
      snprintf(err, sizeof err, "events: too many events (max %d)",
               RUNTIME_EVENTS_MAX_PERF_EVENTS);
      return err;
    }
    perf_counter_configs[count++] = config;
  }
  num_perf_configs = count;
  return NULL;
}

static void perf_events_teardown(struct perf_counters* counters)
{
  /* The mmap reference keeps the perf events alive.
     Unmapping destroys the events - no need to close fds. */
  for (int i = 0; i < counters->ncounters; i++) {
    if (counters->mmap_pages[i] != NULL)
      munmap(counters->mmap_pages[i], sysconf(_SC_PAGESIZE));
  }
}


/* Refresh each counter's rdpmc_info from the kernel's metadata page under
   a seqlock. Called once per counter set during setup, and again from
   perf_events_sample whenever the leader's seqlock has changed.

   Warning: counters->mmap_pages[0..ncounters) must already be populated.

   Returns 0 on success, or 1 if the kernel reports the counter isn't
   RDPMC-readable (cap_user_rdpmc cleared or index == 0, e.g. when too
   many events contend for hardware counter slots). */
static int perf_events_setup_rdpmc(struct perf_counters* counters)
{
  uint32_t seqs[RUNTIME_EVENTS_MAX_PERF_EVENTS];
  struct perf_event_mmap_page* pg;
 retry:
  /* Phase 1: snapshot each counter's seqlock. */
  for (int i = 0; i < counters->ncounters; i++) {
    pg = counters->mmap_pages[i];
    seqs[i] = pg->lock;
  }
  seqlock_barrier();
  /* Phase 2: read the rdpmc parameters. If the kernel is mid-update the
     values may be inconsistent; we'll detect this in Phase 3. */
  for (int i = 0; i < counters->ncounters; i++) {
    pg = counters->mmap_pages[i];
    struct perf_counter_rdpmc_info* info = &counters->rdpmc_info[i];
    info->offset = pg->offset;
    if (!(pg->cap_user_rdpmc && pg->index > 0)) {
      return 1;
    }
    info->index = pg->index - 1;
    info->width = pg->pmc_width;
  }
  seqlock_barrier();
  /* Phase 3: re-read each seqlock. Any change means the kernel re-
     scheduled a counter during Phase 2 and our read is untrustworthy. */
  for (int i = 0; i < counters->ncounters; i++) {
    pg = counters->mmap_pages[i];
    if (pg->lock != seqs[i])
      goto retry;
  }
  counters->last_seq = seqs[0];
  return 0;
}

/* Open the kernel-side perf events for the calling thread, one per
   configured counter. The first is the pinned group leader; the rest
   attach to it so the whole group is scheduled together. On failure
   we record the reason in [err] and break; the caller tears down the
   partially-initialised struct and flips the global-disable flag. */
static int perf_events_setup(struct perf_counters* counters,
                             char* err, size_t err_len)
{
  err[0] = 0;
  memset(counters, 0, sizeof(*counters));
  int leader_fd = -1;

  for (int i = 0; i < num_perf_configs; i++) {
    uint64_t config = perf_counter_configs[i];
    counters->ncounters++;

    struct perf_event_attr attr = {
      .size = sizeof(struct perf_event_attr),
      .type = PERF_TYPE_RAW,
      .config = config,
      .sample_type = PERF_SAMPLE_READ,
      .exclude_kernel = 0,
      .pinned = (i == 0)  /* leader is pinned; group fails cleanly if not */
    };
    int fd =
      syscall(SYS_perf_event_open,
              &attr, 0, -1,
              leader_fd,
              PERF_FLAG_FD_CLOEXEC);
    if (fd < 0) {
      snprintf(err, err_len, "event %d (r%llx): perf_event_open: %s",
               i, (unsigned long long)config, strerror(errno));
      break;
    }
    struct perf_event_mmap_page* page;
    page = mmap(NULL, sysconf(_SC_PAGESIZE), PROT_READ, MAP_SHARED, fd, 0);
    if (page == MAP_FAILED) {
      counters->mmap_pages[i] = NULL;
      close(fd);
      snprintf(err, err_len, "event %d (r%llx): mmap: %s",
               i, (unsigned long long)config, strerror(errno));
      break;
    }
    counters->mmap_pages[i] = page;

    /* Reminder: cap_user_rdpmc can be revoked by the kernel mid-read,
       so probe under the seqlock and retry on a mismatched snapshot. */
    int rdpmc_ok;
    uint32_t seq;
    do {
      seq = page->lock;
      seqlock_barrier();
      rdpmc_ok = page->cap_user_rdpmc && (page->index > 0);
      if (rdpmc_ok) _rdpmc(page->index - 1);
      seqlock_barrier();
    } while (page->lock != seq);

    if (!rdpmc_ok) {
      snprintf(err, err_len,
               "event %d (r%llx): rdpmc reads unavailable (too many counters?)",
               i, (unsigned long long)config);
      close(fd);
      break;
    }

    if (i == 0) {
      leader_fd = fd;
      counters->leader_seq_lock = &page->lock;
      counters->last_seq = (uint32_t)-1;
    } else {
      close(fd);
    }
  }
  /* Close the leader fd - mmap keeps the event alive */
  if (leader_fd != -1) close(leader_fd);
  if (err[0] == 0 && perf_events_setup_rdpmc(counters) != 0) {
    snprintf(err, err_len, "events: failed to setup rdpmc info");
  }
  if (err[0]) {
    perf_events_teardown(counters);
    return -1;
  } else {
    return 0;
  }
}

/* Read each configured counter via userspace RDPMC into [samples]. Hot
   path: called once per sampled event from write_to_ring. The two
   seqlock checks detect kernel re-scheduling between samples; on a
   mismatch we refresh rdpmc_info and retry.

   If perf_events_setup_rdpmc reports rdpmc currently unavailable
   (e.g. the counter has been descheduled because the hardware slots
   are oversubscribed), we write zeros for this event rather than
   spinning: last_seq isn't advanced on failure, so retrying would
   loop forever. We'll automatically recover on a later sample once
   the kernel reschedules the counter.

   Warning: counters must be a fully set-up struct from perf_events_setup,
   and [samples] must have space for at least counters->ncounters words. */
static void perf_events_sample(struct perf_counters* counters,
                               uint64_t* samples)
{
  int n = counters->ncounters;
  if (*counters->leader_seq_lock != counters->last_seq) {
    if (perf_events_setup_rdpmc(counters) != 0) {
      memset(samples, 0, n * sizeof(uint64_t));
      return;
    }
  }
retry:
  for (int i = 0; i < n; i++) {
    struct perf_counter_rdpmc_info info = counters->rdpmc_info[i];
    uint64_t v = _rdpmc(info.index);
    v &= ((uint64_t)(-1)) >> (64 - info.width);
    v += info.offset;
    samples[i] = v;
  }
  seqlock_barrier();
  if (*counters->leader_seq_lock != counters->last_seq) {
    if (perf_events_setup_rdpmc(counters) != 0) {
      memset(samples, 0, n * sizeof(uint64_t));
      return;
    }
    goto retry;
  }
}

static void cleanup_thread_counters(void);

/* Return the calling thread's struct perf_counters, allocating and
   setting it up lazily on first call. Returns NULL if PMC has been
   globally disabled or no counters are configured. */
static struct perf_counters* get_thread_counters(void)
{
  if (atomic_load(&perf_counters_globally_disabled))
    return NULL;

  /* Tear down state from a previous start/destroy epoch. */
  uint64_t current_gen = atomic_load(&perf_counter_generation);
  if (thread_counters != NULL
      && thread_counter_generation != current_gen) {
    cleanup_thread_counters();
  }

  if (thread_counters == NULL && num_perf_configs > 0) {
    thread_counters = caml_stat_alloc(sizeof(struct perf_counters));
    memset(thread_counters, 0, sizeof(struct perf_counters));
    char err[256];
    if (perf_events_setup(thread_counters, err, sizeof err) != 0) {
      caml_gc_log("perf_events setup failed, disabling globally: %s", err);
      atomic_store(&perf_counters_globally_disabled, true);
      caml_stat_free(thread_counters);
      thread_counters = NULL;
      return NULL;
    }
    thread_counter_generation = current_gen;
  }
  return thread_counters;
}

/* Tear down the calling thread's PMC state. */
static void cleanup_thread_counters(void)
{
  if (thread_counters != NULL) {
    perf_events_teardown(thread_counters);
    caml_stat_free(thread_counters);
    thread_counters = NULL;
  }
}
#endif

/* Hook functions for domain and thread lifecycle management.
   These centralize the lifecycle events and PMC initialization/cleanup. */

CAMLexport void caml_runtime_events_domain_start(void)
{
  caml_ev_lifecycle(EV_DOMAIN_SPAWN, getpid());
#ifdef PERF_COUNTERS
  get_thread_counters();
#endif
}

CAMLexport void caml_runtime_events_domain_stop(void)
{
#ifdef PERF_COUNTERS
  cleanup_thread_counters();
#endif
  caml_ev_lifecycle(EV_DOMAIN_TERMINATE, getpid());
}

CAMLexport void caml_runtime_events_thread_start(void)
{
#ifdef PERF_COUNTERS
  get_thread_counters();
#endif
}

CAMLexport void caml_runtime_events_thread_stop(void)
{
#ifdef PERF_COUNTERS
  cleanup_thread_counters();
#endif
}

void caml_runtime_events_init(void) {

  caml_plat_mutex_init(&user_events_lock);
  caml_register_generational_global_root(&user_events);

  runtime_events_path = caml_secure_getenv(T("OCAML_RUNTIME_EVENTS_DIR"));

  if (runtime_events_path) {
    /* caml_secure_getenv's return shouldn't be cached */
    runtime_events_path = caml_stat_strdup_os(runtime_events_path);
  }

  ring_size_words = 1 << caml_params->runtime_events_log_wsize;

  preserve_ring =
            caml_secure_getenv(T("OCAML_RUNTIME_EVENTS_PRESERVE")) ? 1 : 0;

  if (caml_secure_getenv(T("OCAML_RUNTIME_EVENTS_START"))) {
    runtime_events_create_from_stw_single();
    /* stw_single: mutators and domains have not started yet. */
  }
}

/* teardown the ring buffers. This must be called from a stop-the-world
   unless we are sure there is only a single domain running (e.g after a fork)
*/
static void runtime_events_teardown_from_stw_single(int remove_file) {
#ifdef _WIN32
    UnmapViewOfFile(current_metadata);
    CloseHandle(ring_file_handle);
    CloseHandle(ring_handle);

    if( remove_file ) {
      DeleteFile(current_ring_loc);
    }
#else
    /* This cast is necessary for compatibility with Illumos' non-POSIX
      mmap/munmap */
    munmap((void*)current_metadata, current_ring_total_size);

    if( remove_file ) {
      unlink(current_ring_loc);
    }
#endif

    caml_stat_free(current_ring_loc);
    current_metadata = NULL;

#ifdef PERF_COUNTERS
    cleanup_thread_counters();
    num_perf_configs = 0;
    atomic_store(&perf_counters_globally_disabled, false);
    atomic_fetch_add(&perf_counter_generation, 1);
#endif

    atomic_store_release(&runtime_events_enabled, 0);
}

void caml_runtime_events_post_fork(void) {
  /* We are here in the child process after a call to fork (which can only
     happen when there is a single domain) and no mutator code that can spawn a
     new domain can have run yet. Let's be double sure. */
  CAMLassert(caml_domain_alone());

  if (atomic_load_acquire(&runtime_events_enabled)) {
    /* In the child we need to tear down the various structures used for the
    existing runtime_events from the parent. In doing so we need to make sure we
    don't remove the runtime_events file itself as that may still be used by
    the parent. */
    runtime_events_teardown_from_stw_single(0 /* don't remove the file */);
    /* stw_single: mutators and domains have not started after the fork yet. */

    /* We still have the path and ring size from our parent */
    caml_runtime_events_start();
  }
}

/* Return the path of the ring buffers file of this process, or NULL
   if runtime events are not enabled. This is used in the consumer to
   read the ring buffers of the current process. Always returns a
   freshly-allocated string. */
char_os* caml_runtime_events_current_location(void) {
  if( atomic_load_acquire(&runtime_events_enabled) ) {
    return caml_stat_strdup_noexc_os(current_ring_loc);
  } else {
    return NULL;
  }
}

/* Write a lifecycle event and then trigger a stop the world to tear down the
  ring buffers */
void caml_runtime_events_destroy(void) {
  if (atomic_load_acquire(&runtime_events_enabled)) {
    write_to_ring(
      EV_RUNTIME, (ev_message_type){.runtime=EV_LIFECYCLE}, EV_RING_STOP, 0,
      NULL, 0);

    /* clean up runtime_events when we exit if we haven't been instructed to
      preserve the file. */
    int remove_file = preserve_ring ? 0 : 1;
    do {
      caml_try_run_on_all_domains(&stw_teardown_runtime_events,
                                  &remove_file, NULL);
    }
    while( atomic_load_acquire(&runtime_events_enabled) );
  }
}

/* Create the initial runtime_events ring buffers. This must be called from
  within a stop-the-world section if we cannot be sure there is only a single
  domain running. */
static void runtime_events_create_from_stw_single(void) {
  /* Don't initialise runtime_events twice */
  if (!atomic_load_acquire(&runtime_events_enabled)) {
    int ret, ring_headers_length, ring_data_length;
#ifdef _WIN32
    DWORD pid = GetCurrentProcessId();
#else
    int ring_fd;
    long int pid = getpid();
#endif

    current_ring_loc = caml_stat_alloc(RUNTIME_EVENTS_MAX_MSG_LENGTH);

    if (runtime_events_path) {
      snprintf_os(current_ring_loc, RUNTIME_EVENTS_MAX_MSG_LENGTH,
                  T("%s/%ld.events"), runtime_events_path, pid);
    } else {
      snprintf_os(current_ring_loc, RUNTIME_EVENTS_MAX_MSG_LENGTH,
                  T("%ld.events"), pid);
    }

    current_ring_total_size =
        RUNTIME_EVENTS_MAX_CUSTOM_EVENTS *
          sizeof(struct runtime_events_custom_event) +
        caml_params->max_domains * (ring_size_words * sizeof(uint64_t) +
                        sizeof(struct runtime_events_buffer_header)) +
        sizeof(struct runtime_events_metadata_header);

#ifdef _WIN32
    ring_file_handle = CreateFile(
      current_ring_loc,
      GENERIC_READ | GENERIC_WRITE,
      FILE_SHARE_READ | FILE_SHARE_WRITE,
      NULL,
      CREATE_ALWAYS,
      FILE_ATTRIBUTE_NORMAL,
      NULL
    );

    if (ring_file_handle == INVALID_HANDLE_VALUE) {
      char* ring_loc_u8 = caml_stat_strdup_of_os(current_ring_loc);
      if (ring_loc_u8) {
        caml_fatal_error("Couldn't open ring buffer file: %s",
                         ring_loc_u8);
        caml_stat_free(ring_loc_u8);
      } else {
        caml_fatal_error("Couldn't open ring buffer file");
      }
    }

    ring_handle = CreateFileMapping(
      ring_file_handle,
      NULL,
      PAGE_READWRITE,
      0,
      current_ring_total_size,
      NULL
    );

    if (ring_handle == INVALID_HANDLE_VALUE) {
      caml_fatal_error("Could not create file mapping");
    }

    current_metadata = MapViewOfFile(
      ring_handle,
      FILE_MAP_ALL_ACCESS,
      0,
      0,
      0
    );

    if( current_metadata == NULL ) {
      caml_fatal_error("failed to map view of file");
    }
#else
    ring_fd =
        open(current_ring_loc, O_RDWR | O_CREAT, (S_IRUSR | S_IWUSR));

    if (ring_fd < 0) {
      caml_fatal_error("Couldn't open ring buffer loc: %s",
                        current_ring_loc);
    }

    ret = ftruncate(ring_fd, current_ring_total_size);

    if (ret < 0) {
      caml_fatal_error("Can't resize ring buffer");
    }

    /* This cast is necessary for compatibility with Illumos' non-POSIX
      mmap/munmap */
    current_metadata = (struct runtime_events_metadata_header*)
                        mmap(NULL, current_ring_total_size,
                            PROT_READ | PROT_WRITE, MAP_SHARED, ring_fd, 0);

    if (current_metadata == NULL) {
      caml_fatal_error("Unable to mmap ring buffer");
    }

    close(ring_fd);
#endif
    ring_headers_length =
        caml_params->max_domains * sizeof(struct runtime_events_buffer_header);
    ring_data_length =
        caml_params->max_domains * ring_size_words * sizeof(uint64_t);

    current_metadata->version = RUNTIME_EVENTS_VERSION;
    current_metadata->max_domains = caml_params->max_domains;
    current_metadata->ring_header_size_bytes =
        sizeof(struct runtime_events_buffer_header);
    current_metadata->ring_size_bytes =
        ring_size_words * sizeof(uint64_t);
    current_metadata->ring_size_elements = ring_size_words;
    current_metadata->headers_offset =
        sizeof(struct runtime_events_metadata_header);

    /* strictly we can calculate this in a consumer but for simplicity we
        store it in the metadata header */
    current_metadata->data_offset =
      current_metadata->headers_offset + ring_headers_length;
    current_metadata->custom_events_offset =
      current_metadata->data_offset + ring_data_length;


    for (int domain_num = 0; domain_num < caml_params->max_domains;
         domain_num++) {
      /* we initialise each ring's metadata. We use the offset to the headers
         and then find the slot for each of domain in caml_params->max_domains
         */
      struct runtime_events_buffer_header *ring_buffer =
          (struct runtime_events_buffer_header
                *)((char *)current_metadata +
                  current_metadata->headers_offset +
                  domain_num * sizeof(struct runtime_events_buffer_header));

      ring_buffer->ring_head = 0;
      ring_buffer->ring_tail = 0;
    }

    // at the same instant: snapshot user_events list and set
    // runtime_events_enabled to 1
    /* calling from STW */
    caml_plat_lock_blocking(&user_events_lock);
    value current_user_event = user_events;
    atomic_store_release(&runtime_events_enabled, 1);
    caml_plat_unlock(&user_events_lock);

    atomic_store_release(&runtime_events_paused, 0);

    caml_ev_lifecycle(EV_RING_START, pid);

    while (Is_some (current_user_event)) {
      value event = Field(current_user_event, 0);
      events_register_write_buffer(Int_val(Field(event, 0)), Field(event, 1));
      current_user_event = Field(current_user_event, 1);
    }

#ifdef PERF_COUNTERS
    if (num_perf_configs == 0) {
      const char* env =
        caml_secure_getenv(T("OCAML_RUNTIME_EVENTS_PERF_COUNTERS"));
      if (env) {
        const char* err = parse_perf_config_string(env);
        if (err) {
          caml_gc_log("perf_events config parse failed, disabling: %s", err);
          num_perf_configs = 0;
        }
      }
    }
#endif
  }
}

/* create/teardown STWs

   The STW API does have an enter barrier before the handler code is
   run, however, the enter barrier itself calls the runtime events API
   after arrival, which may otherwise race with code inside the STW
   section. Thus, the barrier in the STWs below is needed both to
   ensure that all domains have actually reached the handler before we
   start/stop (to avoid the aforementioned race), and of course to
   ensure that the setup/teardown is observed by all domains returning
   from the STW. */

/* Stop the world section which calls [runtime_events_create_raw], used when we
   can't be sure there is only a single domain running. */
static void stw_create_runtime_events(
  caml_domain_state *domain_state, void *unused,
  int num_participating,
  caml_domain_state **participating_domains)
{
  /* Everyone must be stopped for starting and stopping runtime_events */
  Caml_global_barrier_if_final(num_participating) {
    runtime_events_create_from_stw_single();
  }
}

/* Stop-the-world which calls the teardown code */
static void stw_teardown_runtime_events(
  caml_domain_state *domain_state,
  void *remove_file_data, int num_participating,
  caml_domain_state **participating_domains)
{
  Caml_global_barrier_if_final(num_participating) {
    int remove_file = *(int*)remove_file_data;
    runtime_events_teardown_from_stw_single(remove_file);
  }
}

CAMLexport void caml_runtime_events_start(void) {
  while (!atomic_load_acquire(&runtime_events_enabled)) {
    caml_try_run_on_all_domains(&stw_create_runtime_events, NULL, NULL);
  }
}

CAMLexport void caml_runtime_events_pause(void) {
  if (!atomic_load_acquire(&runtime_events_enabled)) return;

  uintnat not_paused = 0;

  if( atomic_compare_exchange_strong(&runtime_events_paused, &not_paused, 1) ) {
    caml_ev_lifecycle(EV_RING_PAUSE, 0);
  }
}

CAMLexport void caml_runtime_events_resume(void) {
  if (!atomic_load_acquire(&runtime_events_enabled)) return;

  uintnat paused = 1;

  if( atomic_compare_exchange_strong(&runtime_events_paused, &paused, 0) ) {
    caml_ev_lifecycle(EV_RING_RESUME, 0);
  }
}

static inline int ring_is_active(void) {
    return
      atomic_load_relaxed(&runtime_events_enabled)
      && !atomic_load_relaxed(&runtime_events_paused);
}

CAMLexport int caml_runtime_events_are_active(void) {
  return (ring_is_active ());
}

/* Make the three functions above callable from OCaml */

CAMLprim value caml_ml_runtime_events_start(value vunit) {
  caml_runtime_events_start(); return Val_unit;
}

CAMLprim value caml_ml_runtime_events_pause(value vunit) {
  caml_runtime_events_pause(); return Val_unit;
}

CAMLprim value caml_ml_runtime_events_resume(value vunit) {
  caml_runtime_events_resume(); return Val_unit;
}

CAMLprim value caml_ml_runtime_events_path(value vunit)
{
  CAMLparam0();
  CAMLlocal2(res, str);

  if (atomic_load_acquire(&runtime_events_enabled)) {
    /* The allocation might GC, which could allow another domain to
     * nuke current_ring_loc, so we snapshot it first. */

    char_os* current_ring_loc_str = caml_stat_strdup_os(current_ring_loc);

    res = caml_alloc(1, Tag_some);
    str = caml_copy_string_of_os(current_ring_loc_str);
    Store_field(res, 0, str);

    caml_stat_free(current_ring_loc_str);
  } else {
    res = Val_none;
  }

  CAMLreturn(res);
}

CAMLprim value caml_ml_runtime_events_are_active(void) {
  return Val_bool(caml_runtime_events_are_active());
}

CAMLprim value caml_ml_runtime_events_perf_counters_active(value vunit) {
  (void)vunit;
#ifdef PERF_COUNTERS
  /* Force the lazy per-thread setup so the answer reflects whether PMC will
     actually be sampled rather than whether it has been attempted yet. */
  if (num_perf_configs > 0
      && !atomic_load(&perf_counters_globally_disabled)) {
    get_thread_counters();
  }
  return Val_bool(num_perf_configs > 0
                  && !atomic_load(&perf_counters_globally_disabled));
#else
  return Val_false;
#endif
}


static struct runtime_events_buffer_header *get_ring_buffer_by_domain_id
                                                              (int domain_id) {
  return (
      struct runtime_events_buffer_header *)((char *)current_metadata +
                            current_metadata->headers_offset +
                            domain_id *
                                current_metadata->ring_header_size_bytes);
}

static void write_to_ring(ev_category category, ev_message_type type,
                          int event_id, int event_length, uint64_t *content,
                          int word_offset) {

  /* account for header and timestamp (which are both uint64) */
  uint64_t length_with_header_ts = event_length + 2;

  #ifdef PERF_COUNTERS
  /* Only span-edge events carry PMC samples (runtime BEGIN/EXIT and
     user-span Begin/End); everything else writes a 0 in the header's
     perf-counter field. */
  struct perf_counters* counters = get_thread_counters();
  int sample_counters = (counters != NULL && counters->ncounters > 0 &&
                         ((category == EV_RUNTIME &&
                           (type.runtime == EV_BEGIN ||
                            type.runtime == EV_EXIT)) ||
                          (category == EV_USER &&
                           (type.user == EV_USER_MSG_TYPE_SPAN_BEGIN ||
                            type.user == EV_USER_MSG_TYPE_SPAN_END))));

  if (sample_counters) {
    /* Two words per counter: config id and sampled value. */
    length_with_header_ts += counters->ncounters * 2;
  }
  #endif

  /* there is a ring buffer (made up of header and data) for each domain */
  struct runtime_events_buffer_header *domain_ring_header =
      get_ring_buffer_by_domain_id(Caml_state->id);

  /* get the pointer to the data for this domain's ring buffer */
  uint64_t *ring_ptr = (uint64_t *)((char*)current_metadata +
                                    current_metadata->data_offset
                        + Caml_state->id * current_metadata->ring_size_bytes);

  /* the head and tail indexes for the current domain's ring buffer (out of
      the header) */
  uint64_t ring_head = atomic_load_acquire(&domain_ring_header->ring_head);
  uint64_t ring_tail = atomic_load_acquire(&domain_ring_header->ring_tail);

  /* since rings can only be powers of two in size, we use this mask to cheaply
    convert the head and tail indexes in to the physical offset in the ring
    buffer's data. */
  uint64_t ring_mask = current_metadata->ring_size_elements - 1;
  uint64_t ring_tail_offset = ring_tail & ring_mask;

  /* we avoid writing events that straddle the end of the ring buffer */
  uint64_t ring_distance_to_end =
      current_metadata->ring_size_elements - ring_tail_offset;
  /* when we might write an event that is bigger than the physical size
    remaining we add a padding event instead and then write the actual
    event to the start of the ring buffer */
  uint64_t padding_required = 0;

  uint64_t timestamp = caml_time_counter();

  /* length must be less than 2^10 */
  CAMLassert(event_length < RUNTIME_EVENTS_MAX_MSG_LENGTH);
  /* Runtime event with type EV_INTERNAL and id 0 is reserved for padding */
  CAMLassert(
    !(category == EV_RUNTIME && type.runtime == EV_INTERNAL && event_id == 0));

  /* work out if padding is required */
  if (ring_distance_to_end < length_with_header_ts) {
    padding_required = ring_distance_to_end;
  }

  /* First we check if a write would take us over the head */
  while ((ring_tail + length_with_header_ts + padding_required) - ring_head >=
         ring_size_words) {
    /* The write would over-write some old bit of data. Need to advance the
      head. */
    uint64_t head_header = ring_ptr[ring_head & ring_mask];

    ring_head += RUNTIME_EVENTS_ITEM_LENGTH(head_header);

    // advance the ring head
    atomic_store_release(&domain_ring_header->ring_head, ring_head);
  }

  if (padding_required > 0) {
    ring_ptr[ring_tail_offset] =
        (ring_distance_to_end
         << 54); /* Padding header with size ring_distance_to_end
                    Readers will skip the message and go straight
                    to the beginning of the ring. */

    ring_tail += ring_distance_to_end;

    atomic_store_release(&domain_ring_header->ring_tail, ring_tail);

    ring_tail_offset = 0;
  }

#ifdef PERF_COUNTERS
  uint64_t num_perf = sample_counters ? counters->ncounters : 0;
#else
  uint64_t num_perf = 0;
#endif

  /* Below we write the header. See runtime_events.h for the layout structure
     of event headers.
  */

  ring_ptr[ring_tail_offset++] = RUNTIME_EVENTS_HEADER(
                                  length_with_header_ts,
                                  category == EV_RUNTIME,
                                  (type.runtime | type.user),
                                  event_id,
                                  num_perf);

  ring_ptr[ring_tail_offset++] = timestamp;
  if (content != NULL) {
    memcpy(&ring_ptr[ring_tail_offset], content + word_offset,
           event_length * sizeof(uint64_t));
    ring_tail_offset += event_length;
  }

#ifdef PERF_COUNTERS
  if (sample_counters) {
      uint64_t tmp[RUNTIME_EVENTS_MAX_PERF_EVENTS];
      perf_events_sample(counters, tmp);

      char *dest = (char *)&ring_ptr[ring_tail_offset];
      uint64_t counter_length_bytes = num_perf * sizeof(uint64_t);
      memcpy(dest, perf_counter_configs, counter_length_bytes);
      dest += counter_length_bytes;
      memcpy(dest, tmp, counter_length_bytes);
  }
#endif

  atomic_store_release(&domain_ring_header->ring_tail,
                       ring_tail + length_with_header_ts);
}

/* Functions for putting runtime data on to the runtime_events */

void caml_ev_begin(ev_runtime_phase phase) {
  if ( ring_is_active() ) {
    write_to_ring(EV_RUNTIME, (ev_message_type){.runtime=EV_BEGIN}, phase, 0,
                  NULL, 0);
  }
}

void caml_ev_end(ev_runtime_phase phase) {
  if ( ring_is_active() ) {
    write_to_ring(EV_RUNTIME, (ev_message_type){.runtime=EV_EXIT}, phase, 0,
                  NULL, 0);
  }
}

void caml_ev_counter(ev_runtime_counter counter, uint64_t val) {
  if ( ring_is_active() ) {
    uint64_t buf[1];
    buf[0] = val;

    write_to_ring(
      EV_RUNTIME, (ev_message_type){.runtime=EV_COUNTER}, counter, 1, buf, 0);
  }
}

void caml_ev_lifecycle(ev_lifecycle lifecycle, int64_t data) {
  if ( ring_is_active() ) {
    write_to_ring(EV_RUNTIME, (ev_message_type){.runtime=EV_LIFECYCLE},
                  lifecycle, 1, (uint64_t *)&data, 0);
  }
}

static uint64_t alloc_buckets[RUNTIME_EVENTS_NUM_ALLOC_BUCKETS] = { 0, };

void caml_ev_alloc(uint64_t sz) {
  if ( !ring_is_active() )
    return;

  if (sz < 10 * RUNTIME_EVENTS_NUM_ALLOC_BUCKETS_SINGLE) {
    ++alloc_buckets[sz];
  } else if (sz - 10 * RUNTIME_EVENTS_NUM_ALLOC_BUCKETS_SINGLE
             < 10 * RUNTIME_EVENTS_NUM_ALLOC_BUCKETS_DECADE){
    ++alloc_buckets[sz / 10 + 9 * RUNTIME_EVENTS_NUM_ALLOC_BUCKETS_SINGLE];
  } else {
    ++alloc_buckets[RUNTIME_EVENTS_NUM_ALLOC_BUCKETS - 1];
  }
}

void caml_ev_alloc_flush(void) {
  int i;

  if ( !ring_is_active() )
    return;

  write_to_ring(EV_RUNTIME, (ev_message_type){.runtime=EV_ALLOC}, 0,
                  RUNTIME_EVENTS_NUM_ALLOC_BUCKETS, alloc_buckets, 0);

  for (i = 1; i < RUNTIME_EVENTS_NUM_ALLOC_BUCKETS; i++) {
    alloc_buckets[i] = 0;
  }
}

/* Registers the [index] -> [event_name] mapping in the dedicated space in the
   ring buffer */
void events_register_write_buffer(int idx, value event_name) {
  struct runtime_events_custom_event *custom_event =
    &((struct runtime_events_custom_event *)
      ((char *)current_metadata + current_metadata->custom_events_offset))[idx];

  strncpy(custom_event->name, String_val(event_name),
          RUNTIME_EVENTS_CUSTOM_EVENT_ID_LENGTH - 1);
}

CAMLprim value caml_runtime_events_user_register(value event_name,
  value event_tag, value event_type)
{
  CAMLparam3(event_name, event_tag, event_type);
  CAMLlocal2(list_item, event);

  uintnat index = atomic_fetch_add(&runtime_custom_event_index, 1);

  if (index > RUNTIME_EVENTS_MAX_CUSTOM_EVENTS) {
    caml_invalid_argument(
      "Runtime_events.User.register: maximum number of custom events exceeded");
  }

  int length = caml_string_length(event_name);
  if (length > RUNTIME_EVENTS_CUSTOM_EVENT_ID_LENGTH - 1) {
    caml_invalid_argument(
      "Runtime_events.User.register: maximum length for event name exceeded");
  }

  if (!caml_string_is_c_safe(event_name)) {
    caml_invalid_argument(
      "Runtime_events.User.register: event name has null characters");
  }

  // type 'a t = {
  //  id: int;
  //  name: string;
  //  typ: 'a Type.t;
  //  tag: tag;
  //}
  event = caml_alloc_small(4, 0);
  Field(event, 0) = Val_int(index);
  Field(event, 1) = event_name;
  Field(event, 2) = event_type;
  Field(event, 3) = event_tag;


  /* Pre-allocate to avoid STW while holding [user_events_lock]. */
  list_item = caml_alloc(2, 0);

  /* [user_events_lock] can be acquired during STW, so we must use
     caml_plat_lock_blocking and be careful to avoid triggering any
     STW while holding it */
  caml_plat_lock_blocking(&user_events_lock);
  // critical section: when we update the user_events list we need to make sure
  // it is not updated while we construct the pointer to the next element

  if (atomic_load_acquire(&runtime_events_enabled)) {
    // Ring buffer is already available, we register the name
    events_register_write_buffer(index, event_name);
  }

  // event is added to the list of known events
  Store_field(list_item, 0, event);
  Store_field(list_item, 1, user_events);
  caml_modify_generational_global_root(&user_events, list_item);
  // end critical section
  caml_plat_unlock(&user_events_lock);

  CAMLreturn(event);
}

CAMLprim value caml_runtime_events_user_write(
  value write_buffer, value event, value event_content)
{
  CAMLparam3(write_buffer, event, event_content);
  CAMLlocal3(event_id, event_type, res);

  if ( !ring_is_active() )
    CAMLreturn(Val_unit);

  /* event type:
  type 'a t = {
    id: int;
    name: string;
    typ: 'a Type.t;
    tag: 'a tag option;
  }
  */
  event_id = Field(event, 0);

  event_type = Field(event, 2);
  /* event_type type:
  type 'a t =
  | Unit : unit t
  | Int : int t
  | Span : span t
  | Custom : 'a custom -> 'a t
  */

  // Check if event is custom or not.
  if (Is_block(event_type)) {
    // Custom { serialize; deserialize; id }
    value record = Field(event_type, 0);
    value serializer = Field(record, 0);

    res = caml_callback2_exn(serializer, write_buffer, event_content);

    if (Is_exception_result(res)) {
      res = Extract_exception(res);
      caml_raise(res);
    }

    /* Need to check whether the ring is active again as the ring might
     * potentially have been destroyed during the callback. */
    if ( !ring_is_active() )
      CAMLreturn(Val_unit);

    uintnat len_bytes = Int_val(res);
    uintnat len_64bit_word = (len_bytes + sizeof(uint64_t)) / sizeof(uint64_t);
    uintnat offset_index = len_64bit_word * sizeof(uint64_t) - 1;
    Bytes_val(write_buffer)[offset_index] = offset_index - len_bytes;
    write_to_ring(EV_USER, (ev_message_type){.user=EV_USER_MSG_TYPE_CUSTOM},
      Int_val(event_id), len_64bit_word, (uint64_t *) Bytes_val(write_buffer),
      0);

  } else {
    // Unit | Int | Span

    int event_type_id = Int_val(event_type);

    // Unit
    if (event_type_id == EV_USER_ML_TYPE_UNIT) {
      write_to_ring(EV_USER, (ev_message_type){.user=EV_USER_MSG_TYPE_UNIT},
        Int_val(event_id), 0, NULL, 0);
    }

    // Int
    if (event_type_id == EV_USER_ML_TYPE_INT) {
      uint64_t c_event_content = Int_val(event_content);
      write_to_ring(EV_USER, (ev_message_type){.user=EV_USER_MSG_TYPE_INT},
        Int_val(event_id), 1, &c_event_content, 0);
    }

    // Span
    if (event_type_id == EV_USER_ML_TYPE_SPAN) {
      // event_content type is Begin | End
      ev_user_message_type message_type;
      if (Int_val(event_content) == 0) {
        message_type = EV_USER_MSG_TYPE_SPAN_BEGIN;
      } else {
        message_type = EV_USER_MSG_TYPE_SPAN_END;
      }
      write_to_ring(EV_USER, (ev_message_type){.user=message_type},
        Int_val(event_id), 0, NULL, 0);
    }
  }

  CAMLreturn (Val_unit);
}

/* Find which event has the given name using the list of globally known events.
   If the event is not globally known but the type is one of the known types,
   then it can be partially reconstructed, the only missing information being
   the associated tag. This function returns an event structure, except when the
   event is unknown and the event type id is EV_USER_ML_TYPE_CUSTOM. */
CAMLexport value caml_runtime_events_user_resolve(
  char* event_name, ev_user_ml_type event_type_id)
{
  CAMLparam0();
  CAMLlocal3(event, cur_event_name, ml_event_name);

  // TODO: it might be possible to atomic load instead
  caml_plat_lock_blocking(&user_events_lock);
  value current_user_event = user_events;
  caml_plat_unlock(&user_events_lock);

  // which try to find an event with the matching name
  while (Is_some (current_user_event)) {
    event = Field(current_user_event, 0);
    cur_event_name = Field(event, 1);

    if (strncmp(String_val(cur_event_name), event_name,
                RUNTIME_EVENTS_CUSTOM_EVENT_ID_LENGTH) == 0) {
      CAMLreturn(event);
    }

    current_user_event = Field(current_user_event, 1);
  }

  if (event_type_id != EV_USER_ML_TYPE_CUSTOM) {
    // the event is not known, but its type is known
    // as we know the event type the event can be reconstructed
    value event_type = Val_int(event_type_id);
    uintnat event_name_len = strnlen(event_name,
                                      RUNTIME_EVENTS_CUSTOM_EVENT_ID_LENGTH);
    ml_event_name = caml_alloc_initialized_string(event_name_len, event_name);
    event = caml_runtime_events_user_register(ml_event_name, Val_none,
                                              event_type);

    CAMLreturn(event);
  }

  CAMLreturn (Val_none);
}

/* Linker compatibility with unused 4 stdlib externals */

CAMLprim value caml_eventlog_resume(value v)
{
  caml_failwith("Called caml_eventlog_resume in runtime5: not supported.");
}

CAMLprim value caml_eventlog_pause(value v)
{
  caml_failwith("Called caml_eventlog_pause in runtime5: not supported.");
}
