# Location of the ESMF makefile fragment for this component:
ww3_mk = $(WW3_BINDIR)/nuopc.mk
all_component_mk_files+=$(ww3_mk)

# Location of source code and installation
WW3_SRCDIR?=$(ROOTDIR)/WW3/model
WW3_BINDIR?=$(ROOTDIR)/WW3_INSTALL
WW3_ENVFIL?=$(WW3_SRCDIR)/bin/wwatch3.env

# Make sure the source directory exists and is non-empty
$(call require_dir,$(WW3_SRCDIR),WW3 source directory)

# Rule for building this component:
build_WW3: $(ww3_mk)

override STRLEN_WW3_SRCDIR=$(call strlen,$(WW3_SRCDIR))
override MAX_LEN_WW3_SRCDIR=400

ifeq ($(true),$(call int_gt,$(call int_encode,$(STRLEN_WW3_SRCDIR)),$(call int_encode,$(MAX_LEN_WW3_SRCDIR))))
  $(warning WW3_SRCDIR path too long: $(WW3_SRCDIR))
  $(error WW3_SRCDIR path length $(STRLEN_WW3_SRCDIR) is longer than maximum allowed $(MAX_LEN_WW3_SRCDIR) chars)
endif

WW3_CONFOPT ?= $(FULL_MACHINE_ID)

override WW3_CONF_FILE = $(WW3_SRCDIR)/bin/comp.tmpl

ifeq (,$(wildcard $(WW3_CONF_FILE)))
  $(warning $(WW3_CONF_FILE): no such file or directory)
  $(error WW3 config now uses cmplr.env and comp/link.tmpl. $(WW3_CONFOPT) not found, check the WW3 submodule.)
endif

WW3_ALL_OPTS= \
  COMP_SRCDIR="$(WW3_SRCDIR)" \
  COMP_BINDIR="$(WW3_BINDIR)" \
  WW3_COMP="$(WW3_CONFOPT)" \
  WW3_F90="$(F90)" \
  WW3_CC="$(CC)"

$(ww3_mk): configure
	+$(MODULE_LOGIC) ; set -x ; cd $(WW3_SRCDIR)/esmf       ; \
	export $(WW3_ALL_OPTS); export WWATCH3_ENV=$(WW3_ENVFIL) ; \
	echo "#"                                                       > $(WW3_ENVFIL) ; \
	echo "# Environment variables for wavewatch III (CoastalApp)" >> $(WW3_ENVFIL) ; \
	echo "# ----------------------------------------------------" >> $(WW3_ENVFIL) ; \
	echo "#"                                                      >> $(WW3_ENVFIL) ; \
	echo ""                                                       >> $(WW3_ENVFIL) ; \
	echo "WWATCH3_LPR"                                            >> $(WW3_ENVFIL) ; \
	echo "WWATCH3_F90      $(F90)"                                >> $(WW3_ENVFIL) ; \
	echo "WWATCH3_CC       $(CC)"                                 >> $(WW3_ENVFIL) ; \
	echo "WWATCH3_DIR      $(WW3_SRCDIR)"                         >> $(WW3_ENVFIL) ; \
	echo "WWATCH3_TMP      $(WW3_SRCDIR)/tmp"                     >> $(WW3_ENVFIL) ; \
	echo "WWATCH3_SOURCE   yes"                                   >> $(WW3_ENVFIL) ; \
	echo "WWATCH3_LIST     yes"                                   >> $(WW3_ENVFIL) ; \
	exec $(MAKE) -j 1 WW3_COMP="$(WW3_CONFOPT)" COMP_BINDIR="$(WW3_BINDIR)" WW3_F90="$(F90)" WW3_CC="$(CC)" ww3_nems
	mkdir -p $(WW3_BINDIR)
	cp $(WW3_SRCDIR)/nuopc.mk $(WW3_BINDIR)/.
	test -d "$(WW3_BINDIR)"

# Rule for cleaning the SRCDIR and BINDIR:
clean_WW3:
	+$(MODULE_LOGIC) ; cd $(WW3_SRCDIR)/esmf                ; \
	export $(WW3_ALL_OPTS)                                  ; \
	export ESMFMKFILE=/dev/null                             ; \
	exec $(MAKE) distclean
	cd "$(WW3_SRCDIR)"                                      ; \
	rm -rf exe mod* obj* tmp ftn/makefile ftn/makefile_DIST   \
	   ftn/makefile_SHRD esmf/wwatch3.env aux/makefile        \
	   bin/link bin/comp                                    ; \
	find . -name '*.o' -o -name '*.mod' -o -name '*.a'        \
	  | xargs rm -f

distclean_WW3: clean_WW3
	rm -rf $(WW3_BINDIR) $(ww3_mk)
