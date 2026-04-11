BYTE_FILES=bytes.cmi bytes.cma
NATIVE_FILES=bytes.cmx bytes$(LIB_SUFFIX) bytes.cmxa
NATIVE_FILES_DYNLINK=bytes.cmxs

TOP=../..
include $(TOP)/Makefile.config

OCAMLC = ocamlc
OCAMLOPT = ocamlopt $(OCAMLOPT_G)
OCAMLOPT_SHARED = $(OCAMLOPT)

build: all opt

all:
	$(OCAMLC) -a -o bytes.cma bytes.ml

opt:
	$(OCAMLOPT) -a -o bytes.cmxa bytes.ml
	if [ $(HAVE_NATDYNLINK) -gt 0 ]; then \
	    $(OCAMLOPT_SHARED) -shared -o bytes.cmxs bytes.cmxa; \
	fi

install: all
	$(INSTALLDIR) "$(DESTDIR)$(prefix)$(OCAML_SITELIB)/bytes"
	$(CP) META $(BYTE_FILES) "$(DESTDIR)$(prefix)$(OCAML_SITELIB)/bytes/"
	for f in $(NATIVE_FILES) $(NATIVE_FILES_DYNLINK); do if [ -f "$$f" ]; then $(CP) $$f "$(DESTDIR)$(prefix)$(OCAML_SITELIB)/bytes/"; fi; done

uninstall: 
	rm -rf "$(DESTDIR)$(prefix)$(OCAML_SITELIB)/bytes"

# install-self and uninstall-self use ocamlfind already. This is a bit
# questionable here.

install-self: all
	ocamlfind install bytes META $(BYTE_FILES) -optional $(NATIVE_FILES) $(NATIVE_FILES_DYNLINK)

uninstall-self:
	ocamlfind remove bytes

clean:
	ocamlbuild -clean
