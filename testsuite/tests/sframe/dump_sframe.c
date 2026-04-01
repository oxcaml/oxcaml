/* Standalone SFrame v2 section dumper for ELF object files.
   Usage: dump_sframe <file.o>

   Reads the .sframe section from an ELF object file and prints a
   human-readable dump of the header, FDEs, and FREs. */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <elf.h>

/* ---- SFrame v2 format definitions ---- */

#define SFRAME_MAGIC           0xdee2
#define SFRAME_VERSION_2       2

#define SFRAME_F_FDE_SORTED           0x1
#define SFRAME_F_FRAME_POINTER        0x2
#define SFRAME_F_FDE_FUNC_START_PCREL 0x4

#define SFRAME_ABI_AARCH64_ENDIAN_BIG    1
#define SFRAME_ABI_AARCH64_ENDIAN_LITTLE 2
#define SFRAME_ABI_AMD64_ENDIAN_LITTLE   3

#define SFRAME_FDE_TYPE_PCINC  0

#define SFRAME_FRE_TYPE_ADDR1  0
#define SFRAME_FRE_TYPE_ADDR2  1
#define SFRAME_FRE_TYPE_ADDR4  2

#define SFRAME_FRE_OFFSET_1B   0
#define SFRAME_FRE_OFFSET_2B   1
#define SFRAME_FRE_OFFSET_4B   2

/* SFrame header (28 bytes) */
struct sframe_header {
  uint16_t sfh_preamble_magic;
  uint8_t  sfh_preamble_version;
  uint8_t  sfh_preamble_flags;
  uint8_t  sfh_abi_arch;
  int8_t   sfh_cfa_fixed_fp_offset;
  int8_t   sfh_cfa_fixed_ra_offset;
  uint8_t  sfh_auxhdr_len;
  uint32_t sfh_num_fdes;
  uint32_t sfh_num_fres;
  uint32_t sfh_fre_len;
  uint32_t sfh_fdeoff;
  uint32_t sfh_freoff;
};

/* SFrame FDE (20 bytes) */
struct sframe_fde {
  int32_t  sfde_func_start_address;
  uint32_t sfde_func_size;
  uint32_t sfde_func_start_fre_off;
  uint32_t sfde_func_num_fres;
  uint8_t  sfde_func_info;
  uint8_t  sfde_func_rep_size;
  uint16_t sfde_func_padding2;
};

/* ---- Helpers ---- */

static const char *abi_name(uint8_t abi) {
  switch (abi) {
    case SFRAME_ABI_AARCH64_ENDIAN_BIG:    return "AARCH64_BIG";
    case SFRAME_ABI_AARCH64_ENDIAN_LITTLE: return "AARCH64_LITTLE";
    case SFRAME_ABI_AMD64_ENDIAN_LITTLE:   return "AMD64_LITTLE";
    default: return "unknown";
  }
}

static void print_flags(uint8_t flags) {
  int first = 1;
  if (flags & SFRAME_F_FDE_SORTED)
    { printf("%sFDE_SORTED", first ? "" : ","); first = 0; }
  if (flags & SFRAME_F_FRAME_POINTER)
    { printf("%sFRAME_POINTER", first ? "" : ","); first = 0; }
  if (flags & SFRAME_F_FDE_FUNC_START_PCREL)
    { printf("%sFDE_FUNC_START_PCREL", first ? "" : ","); first = 0; }
  if (first) printf("NONE");
}

/* Find a section by name in an ELF64 file. Returns pointer to section
   data within the mapped buffer, or NULL. Sets *out_size. */
static const uint8_t *find_section(const uint8_t *buf, size_t buflen,
                                   const char *name, size_t *out_size) {
  if (buflen < sizeof(Elf64_Ehdr)) return NULL;
  const Elf64_Ehdr *ehdr = (const Elf64_Ehdr *)buf;

  if (memcmp(ehdr->e_ident, ELFMAG, SELFMAG) != 0) {
    fprintf(stderr, "Not an ELF file\n");
    return NULL;
  }
  if (ehdr->e_ident[EI_CLASS] != ELFCLASS64) {
    fprintf(stderr, "Not a 64-bit ELF file\n");
    return NULL;
  }

  if (ehdr->e_shoff == 0 || ehdr->e_shnum == 0) return NULL;
  if (ehdr->e_shstrndx == SHN_UNDEF) return NULL;

  const Elf64_Shdr *shdrs = (const Elf64_Shdr *)(buf + ehdr->e_shoff);
  const Elf64_Shdr *strtab_sh = &shdrs[ehdr->e_shstrndx];
  const char *strtab = (const char *)(buf + strtab_sh->sh_offset);

  for (int i = 0; i < ehdr->e_shnum; i++) {
    const char *sec_name = strtab + shdrs[i].sh_name;
    if (strcmp(sec_name, name) == 0) {
      *out_size = shdrs[i].sh_size;
      return buf + shdrs[i].sh_offset;
    }
  }
  return NULL;
}

/* ---- Main dump logic ---- */

static int dump_fre(const uint8_t *fre_data, size_t fre_avail,
                    uint8_t fre_type, int is_amd64) {
  /* Read start address */
  uint32_t start_addr;
  size_t addr_size;
  switch (fre_type) {
    case SFRAME_FRE_TYPE_ADDR1: addr_size = 1; break;
    case SFRAME_FRE_TYPE_ADDR2: addr_size = 2; break;
    case SFRAME_FRE_TYPE_ADDR4: addr_size = 4; break;
    default:
      fprintf(stderr, "    unknown FRE address type %d\n", fre_type);
      return -1;
  }
  if (fre_avail < addr_size + 1) return -1;

  start_addr = 0;
  memcpy(&start_addr, fre_data, addr_size);
  const uint8_t *p = fre_data + addr_size;

  /* Info byte */
  uint8_t info = *p++;
  int base_reg_id   = info & 0x1;
  int offset_count  = (info >> 1) & 0xf;
  int offset_size   = (info >> 5) & 0x3;
  int mangled_ra    = (info >> 7) & 0x1;

  size_t off_bytes;
  switch (offset_size) {
    case SFRAME_FRE_OFFSET_1B: off_bytes = 1; break;
    case SFRAME_FRE_OFFSET_2B: off_bytes = 2; break;
    case SFRAME_FRE_OFFSET_4B: off_bytes = 4; break;
    default: return -1;
  }

  size_t total = addr_size + 1 + offset_count * off_bytes;
  if (fre_avail < total) return -1;

  /* Read offsets as signed values */
  int32_t offsets[15];
  for (int i = 0; i < offset_count; i++) {
    int32_t val = 0;
    memcpy(&val, p, off_bytes);
    /* Sign-extend for 1-byte and 2-byte */
    if (off_bytes == 1 && (val & 0x80)) val |= ~0xff;
    if (off_bytes == 2 && (val & 0x8000)) val |= ~0xffff;
    offsets[i] = val;
    p += off_bytes;
  }

  printf("    FRE: start_addr=+%u, %s", start_addr,
         base_reg_id ? "FP" : "SP");

  /* Offset interpretation depends on ABI.
     AMD64: offset[0]=CFA, offset[1]=FP (optional). RA is fixed.
     AArch64: offset[0]=CFA, offset[1]=RA (optional),
              offset[2]=FP (optional). */
  int idx = 0;
  if (idx < offset_count)
    printf(", cfa_offset=%+d", offsets[idx++]);

  if (is_amd64) {
    if (idx < offset_count)
      printf(", fp_offset=%+d", offsets[idx++]);
  } else {
    if (idx < offset_count)
      printf(", ra_offset=%+d", offsets[idx++]);
    if (idx < offset_count)
      printf(", fp_offset=%+d", offsets[idx++]);
  }

  if (mangled_ra) printf(" [signed-ra]");
  printf("\n");

  return (int)total;
}

static void dump_sframe(const uint8_t *data, size_t size) {
  if (size < sizeof(struct sframe_header)) {
    fprintf(stderr, "SFrame section too small for header\n");
    return;
  }

  struct sframe_header hdr;
  memcpy(&hdr, data, sizeof(hdr));

  if (hdr.sfh_preamble_magic != SFRAME_MAGIC) {
    fprintf(stderr, "Bad SFrame magic: 0x%04x\n", hdr.sfh_preamble_magic);
    return;
  }

  int is_amd64 = (hdr.sfh_abi_arch == SFRAME_ABI_AMD64_ENDIAN_LITTLE);

  printf("SFrame header:\n");
  printf("  Version: %d\n", hdr.sfh_preamble_version);
  printf("  Flags: ");
  print_flags(hdr.sfh_preamble_flags);
  printf("\n");
  printf("  ABI/arch: %s\n", abi_name(hdr.sfh_abi_arch));
  printf("  Fixed FP offset: %d\n", hdr.sfh_cfa_fixed_fp_offset);
  printf("  Fixed RA offset: %d\n", hdr.sfh_cfa_fixed_ra_offset);
  printf("  Num FDEs: %u\n", hdr.sfh_num_fdes);
  printf("  Num FREs: %u\n", hdr.sfh_num_fres);
  printf("  FRE section length: %u\n", hdr.sfh_fre_len);

  const uint8_t *after_hdr = data + sizeof(struct sframe_header)
                             + hdr.sfh_auxhdr_len;
  const uint8_t *fde_base = after_hdr + hdr.sfh_fdeoff;
  const uint8_t *fre_base = after_hdr + hdr.sfh_freoff;

  for (uint32_t i = 0; i < hdr.sfh_num_fdes; i++) {
    struct sframe_fde fde;
    memcpy(&fde, fde_base + i * sizeof(struct sframe_fde),
           sizeof(struct sframe_fde));

    uint8_t fre_type = fde.sfde_func_info & 0xf;
    uint8_t fde_type = (fde.sfde_func_info >> 4) & 0x1;

    printf("  FDE %u: size=%u, num_fres=%u, type=%s, fre_type=ADDR%d\n",
           i, fde.sfde_func_size, fde.sfde_func_num_fres,
           fde_type == SFRAME_FDE_TYPE_PCINC ? "PCINC" : "PCMASK",
           fre_type == SFRAME_FRE_TYPE_ADDR1 ? 1 :
           fre_type == SFRAME_FRE_TYPE_ADDR2 ? 2 : 4);

    const uint8_t *fre_ptr = fre_base + fde.sfde_func_start_fre_off;
    size_t fre_remain = (fre_base + hdr.sfh_fre_len) - fre_ptr;

    for (uint32_t j = 0; j < fde.sfde_func_num_fres; j++) {
      int consumed = dump_fre(fre_ptr, fre_remain, fre_type, is_amd64);
      if (consumed < 0) {
        fprintf(stderr, "    (error reading FRE %u)\n", j);
        break;
      }
      fre_ptr += consumed;
      fre_remain -= consumed;
    }
  }
}

/* ---- Entry point ---- */

int main(int argc, char **argv) {
  if (argc != 2) {
    fprintf(stderr, "Usage: %s <elf-file>\n", argv[0]);
    return 1;
  }

  FILE *f = fopen(argv[1], "rb");
  if (!f) { perror(argv[1]); return 1; }

  fseek(f, 0, SEEK_END);
  long fsize = ftell(f);
  fseek(f, 0, SEEK_SET);

  uint8_t *buf = malloc(fsize);
  if (!buf) { perror("malloc"); fclose(f); return 1; }
  if (fread(buf, 1, fsize, f) != (size_t)fsize) {
    perror("fread"); free(buf); fclose(f); return 1;
  }
  fclose(f);

  size_t sf_size = 0;
  const uint8_t *sf_data = find_section(buf, fsize, ".sframe", &sf_size);
  if (!sf_data) {
    fprintf(stderr, "No .sframe section found\n");
    free(buf);
    return 1;
  }

  dump_sframe(sf_data, sf_size);

  free(buf);
  return 0;
}
