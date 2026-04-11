# make all: compile to bytecode
# make opt: compile to native code
# make install: install bytecode and/or native code
#----------------------------------------------------------------------

include Makefile.config
-include Makefile.packages

TOP=.

.PHONY: all opt install uninstall clean

all:
	for p in $(PARTS); do ( cd src/$$p; $(MAKE) all ) || exit; done
	$(MAKE) all-config

opt:
	for p in $(PARTS); do ( cd src/$$p; $(MAKE) opt ) || exit; done

install: check-installation
	$(INSTALLDIR) "$(DESTDIR)$(prefix)$(OCAMLFIND_BIN)"
	$(INSTALLDIR) "$(DESTDIR)$(prefix)$(OCAMLFIND_MAN)"
	$(MAKE) install-config
	for p in $(PARTS); do ( cd src/$$p; $(MAKE) install ); done
	$(MAKE) install-meta
	test ! -f 'site-lib-src/num-top/META' || { cd src/findlib; $(MAKE) install-num-top; }
	test ! -f 'site-lib-src/camlp4/META' ||	$(INSTALLFILE) tools/safe_camlp4 "$(DESTDIR)$(prefix)$(OCAMLFIND_BIN)"
	$(MAKE) install-doc

uninstall: check-installation
	$(MAKE) uninstall-doc
	$(MAKE) uninstall-meta
	for p in `cd src; echo *`; do ( cd src/$$p; $(MAKE) uninstall ); done
	$(MAKE) uninstall-config

clean:
	for p in `cd src; echo *`; do ( cd src/$$p; $(MAKE) clean ); done
	(cd itest-aux; $(MAKE) clean)
	(cd tools/extract_args; $(MAKE) clean)
	rm -f findlib.conf Makefile.packages

.PHONY: release
release: README
	./release

README: doc/README
	ln -s doc/README .


.PHONY: all-config
all-config: findlib.conf

.PHONY: findlib-template
findlib-template: findlib.conf.in
	USE_CYGPATH="$(USE_CYGPATH)"; \
	export USE_CYGPATH; \
	cat findlib.conf.in | \
	    $(SH) tools/patch '@SITELIB@' '$(FINDLIB_OCAML_SITELIB)' | \
			$(SH) tools/patch '@FINDLIB_PATH@' '$(FINDLIB_PATH)' -p >findlib.conf
	if ./tools/cmd_from_same_dir ocamlc; then \
		echo 'ocamlc="ocamlc.opt"' >>findlib.conf; \
	fi
	if ./tools/cmd_from_same_dir ocamlopt; then \
		echo 'ocamlopt="ocamlopt.opt"' >>findlib.conf; \
	fi
	if ./tools/cmd_from_same_dir ocamldep; then \
		echo 'ocamldep="ocamldep.opt"' >>findlib.conf; \
	fi
	if ./tools/cmd_from_same_dir ocamldoc; then \
		echo 'ocamldoc="ocamldoc.opt"' >>findlib.conf; \
	fi

.PHONY: findlib-relative
findlib-relative: FINDLIB_OCAML_SITELIB=$(RELATIVE_OCAML_SITELIB)
findlib-relative: findlib-template

.PHONY: findlib-absolute
findlib-absolute: FINDLIB_OCAML_SITELIB=$(OCAML_SITELIB)
findlib-absolute: findlib-template

findlib.conf: findlib.conf.in
	if [ "$(RELATIVE_PATHS)" = "true" ]; then \
	  $(MAKE) findlib-relative; \
        else \
	  $(MAKE) findlib-absolute; \
	fi

.PHONY: install-doc
install-doc:
	$(INSTALLDIR) "$(DESTDIR)$(prefix)$(OCAMLFIND_MAN)/man1" "$(DESTDIR)$(prefix)$(OCAMLFIND_MAN)/man3" "$(DESTDIR)$(prefix)$(OCAMLFIND_MAN)/man5"
	-$(CP) doc/ref-man/ocamlfind.1 "$(DESTDIR)$(prefix)$(OCAMLFIND_MAN)/man1"
	-$(CP) doc/ref-man/META.5 doc/ref-man/site-lib.5 doc/ref-man/findlib.conf.5 "$(DESTDIR)$(prefix)$(OCAMLFIND_MAN)/man5"

.PHONY: uninstall-doc
uninstall-doc:
	rm -f "$(DESTDIR)$(prefix)$(OCAMLFIND_MAN)/man1/ocamlfind.1"
	rm -f "$(DESTDIR)$(prefix)$(OCAMLFIND_MAN)/man3/Findlib.3"
	rm -f "$(DESTDIR)$(prefix)$(OCAMLFIND_MAN)/man3/Topfind.3"
	rm -f "$(DESTDIR)$(prefix)$(OCAMLFIND_MAN)/man5/META.5"
	rm -f "$(DESTDIR)$(prefix)$(OCAMLFIND_MAN)/man5/site-lib.5"


.PHONY: check-installation
check-installation:
	if [ "$(CHECK_BEFORE_INSTALL)" -eq 1 ]; then \
    for x in camlp4 dbm graphics labltk num ocamlbuild; do \
      if [ -f "$(prefix)$(OCAML_SITELIB)/$$x/META" ]; then \
        if ! grep -Fq '[distributed with Ocaml]' "$(prefix)/$(OCAML_SITELIB)/$$x/META"; then \
          rm -f site-lib-src/$$x/META; \
        fi; \
      fi; \
    done; \
    test -f "site-lib-src/num/META" || rm -f "site-lib-src/num-top/META"; \
  fi
	echo 'SITELIB_META =' > Makefile.packages.in
	for x in `ls site-lib-src`; do test ! -f "site-lib-src/$$x/META" || echo $$x >> Makefile.packages.in; done
	tr '\n' ' ' < Makefile.packages.in > Makefile.packages
	rm Makefile.packages.in

.PHONY: install-meta
install-meta:
	for x in $(SITELIB_META); do $(INSTALLDIR) "$(DESTDIR)$(prefix)$(OCAML_SITELIB)/$$x"; $(CP) site-lib-src/$$x/META "$(DESTDIR)$(prefix)$(OCAML_SITELIB)/$$x/META.tmp" && mv "$(DESTDIR)$(prefix)$(OCAML_SITELIB)/$$x/META.tmp" "$(DESTDIR)$(prefix)$(OCAML_SITELIB)/$$x/META"; done
	$(INSTALLDIR) "$(DESTDIR)$(prefix)$(OCAML_SITELIB)/findlib"; $(CP) Makefile.packages "$(DESTDIR)$(prefix)$(OCAML_SITELIB)/findlib/Makefile.packages"

.PHONY: uninstall-meta
uninstall-meta:
	for x in $(SITELIB_META); do rm -rf "$(DESTDIR)$(prefix)$(OCAML_SITELIB)/$$x"; done

.PHONY: install-config
install-config:
	$(INSTALLDIR) "`dirname \"$(DESTDIR)$(prefix)$(OCAMLFIND_CONF)\"`"
	@if [ -f "$(DESTDIR)$(prefix)$(OCAMLFIND_CONF)" ]; then echo "!!! Keeping old $(DESTDIR)$(prefix)$(OCAMLFIND_CONF) !!!"; fi
	test -f "$(DESTDIR)$(prefix)$(OCAMLFIND_CONF)" || $(CP) findlib.conf "$(DESTDIR)$(prefix)$(OCAMLFIND_CONF)"

.PHONY: uninstall-config
uninstall-config:
	@echo Leaving "$(OCAMLFIND_CONF)" installed, consider manual removal

.PHONY: interface-lists
interface-lists:
	d=`ocamlc -where`;                              \
	for x in `ls site-lib-src`; do                  \
	    iflist="";                                  \
            if [ ! -f "site-lib-src/$$x/interfaces.in" ]; then continue; fi; \
	    cma_spec=`cat site-lib-src/$$x/interfaces.in`;  \
	    for cma in $$d/$$cma_spec; do               \
		intf=`ocamlobjinfo $$cma |                   \
		      grep 'Unit name:' |               \
		      sed -e 's/^  Unit name: //' |     \
		      sort |                            \
		      tr '\n' ' '`;                     \
		iflist="$$iflist $$intf";               \
	    done;                                       \
	    echo "$$iflist" >"site-lib-src/$$x/interfaces.out"; \
	done

######################################################################
# The following is from Pietro Abata <pietro.abate@anu.edu.au>
# to create MacOS X packages. I did not test it, just include it.

.PHONY: package-macosx

package-macosx: all opt
	$(INSTALLDIR) package-macosx/root
	export prefix=`pwd`/package-macosx/root && make install
	export VERSION=1.1.2 && sh tools/make-package-macosx

clean-macosx:
	sudo rm -rf package-macosx
