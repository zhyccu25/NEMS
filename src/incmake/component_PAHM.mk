###########################################################################
### NUOPC GNUMake Macro File for the Parametric Hurricane Model (PaHM)
###
### Author: Panagiotis Velissariou <panagiotis.velissariou@noaa.gov>
### Date:   June 25 2021
###########################################################################


########################################################################

# Location of the ESMF makefile fragment for this component:
pahm_mk = $(PAHM_BINDIR)/pahm.mk
all_component_mk_files+=$(pahm_mk)

# Location of source code and installation
PAHM_SRCDIR?=$(ROOTDIR)/PAHM
PAHM_BINDIR?=$(ROOTDIR)/PAHM_INSTALL

# Make sure the expected directories exist and are non-empty:
$(call require_dir,$(PAHM_SRCDIR),PAHM source directory)

########################################################################

# Rule for building this component:

build_PAHM: $(pahm_mk)

$(pahm_mk):
	+cd $(PAHM_SRCDIR); exec ./build.sh --compiler=$(NEMS_COMPILER) --plat=$(NEMS_PLATFORM) \
	   --parallel=${NEMS_PARALLEL} --prefix=$(PAHM_BINDIR) --verbose=1
#	+cd $(PAHM_SRCDIR); exec ./build.sh --compiler=$(NEMS_COMPILER) --plat=$(NEMS_PLATFORM) \
#	   --prefix=$(PAHM_BINDIR) --verbose=1
	@if [ $$? -eq 0 ]; \
	then \
	  cd $(PAHM_SRCDIR)/nuopc; exec $(MAKE) nuopcinstall \
	     DESTDIR=/ "INSTDIR=$(PAHM_BINDIR)"; \
	  echo ""; \
	  test -d "$(PAHM_BINDIR)"; \
	  echo ""; \
	  test -s $(pahm_mk); \
	  echo ""; \
	else \
          echo "ERROR: could not compile the NUOPC Cap due to PaHM build errors"; \
	  exit 1; \
	fi

########################################################################

# Rule for cleaning the SRCDIR and BINDIR:

clean_PAHM_NUOPC:
	+cd $(PAHM_SRCDIR)/nuopc; exec $(MAKE) nuopcclean
	@echo ""

distclean_PAHM_NUOPC:
	+cd $(PAHM_SRCDIR)/nuopc; exec $(MAKE) nuopcdistclean
	@echo ""

clean_PAHM: clean_PAHM_NUOPC
	+cd $(PAHM_SRCDIR); exec ./build.sh --clean=1
	@echo ""

distclean_PAHM: distclean_PAHM_NUOPC
	+cd $(PAHM_SRCDIR); exec ./build.sh --clean=2
	rm -rf $(PAHM_BINDIR)
	@echo ""
