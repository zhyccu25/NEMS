########################################################################

# This is a template for a component makefile fragment.  In this case,
# the component is "FOO" and so the file would be renamed to
# component_FOO.mk.

# The build and clean rules are set to those of the "build_std"
# function in the old NEMSAppBuilder.  For a different build system,
# the $(foo_mk) and clean_FOO rules.

# Note that in the build and clean rules, any shell variables must
# have double dollar signs ($$) to prevent Make from interpreting them.

# Also note that the commands in the build or clean rules must be a
# single valid shell command line, hence the "; \" at the end of every
# statement.

# clean_FOO: 
# 	something $$happens ; \
# 	something $$else $$happens

# The correct way to run a submake is this:
#
#   +$(MODULE_LOGIC) ; exec $(MAKE) -C $(FOO_SRCDIR) $(FOO_ALL_OPTS) target
#
# If you don't need modules, and you don't care if the "make" succeeds
# (such as when you're cleaning), then this works:
#
#   +-$(MAKE) -C $(FOO_SRCDIR) $(FOO_ALL_OPTS) clean
#
# Meanings of those characters:
#
#   +      = this rule runs "make." Ensures job server (-j) is passed on
#   -      = ignore the exit status of this rule (as if -k was used)
#   $(MAKE) = the path to the "make" program used to run this makefile
#   $(MODULE_LOGIC) = load modules or source the modulefile, if relevant
#   exec $(MAKE)    = the shell process is replaced by "make."  This is
#                     needed to correctly pass on the job server information
#   -C $(FOO_SRCDIR) = cd to $(FOO_SRCDIR) before running make
#   $(FOO_ALL_OPTS)  = pass on all FOO options from the $(FOO_ALL_OPTS) variable
#   target or clean  = the "make" target to build

########################################################################

# Location of the ESMF makefile fragment for this component:
fvcom_mk = $(FVCOM_BINDIR)/fvcom.mk
all_component_mk_files+=$(fvcom_mk)

# Location of source code and installation
FVCOM_SRCDIR?=$(ROOTDIR)/FVCOM
FVCOM_BINDIR?=$(ROOTDIR)/FVCOM_INSTALL

# Make sure the expected directories exist and are non-empty:
$(call require_dir,$(FVCOM_SRCDIR),FVCOM source directory)

FVCOM_ALL_OPTS= \
  COMP_SRCDIR="$(FVCOM_SRCDIR)" \
  COMP_BINDIR="$(FVCOM_BINDIR)" \
  MACHINE_ID="$(MACHINE_ID)"

########################################################################

# Rule for building this component:

build_FVCOM: $(fvcom_mk)

$(fvcom_mk): configure
	+$(MODULE_LOGIC) 
	cd $(FVCOM_SRCDIR)/src ; exec $(MAKE) libfvcom $(FVCOM_ALL_OPTS)
	+$(MODULE_LOGIC) 
	cd $(FVCOM_SRCDIR)/nuopc ; exec $(MAKE)  $(FVCOM_ALL_OPTS)  \
	  DESTDIR=/ "INSTDIR=$(FVCOM_BINDIR)" nuopcinstall
	test -d "$(FVCOM_BINDIR)"
	test -s $(fvcom_mk)

########################################################################

# Rule for cleaning the SRCDIR and BINDIR:

clean_FVCOM:
	+cd $(FVCOM_SRCDIR)/src ; exec $(MAKE) -k clean
	+cd $(FVCOM_SRCDIR)/nuopc ; exec rm -f *.o *.mod libfvcom_cap.a

distclean_FVCOM: clean_FVCOM
	rm -rf $(FVCOM_BINDIR) $(fvcom_mk)

distclean_NUOPC:
	+cd $(FVCOM_SRCDIR)/cpl/nuopc ; exec rm -f *.o *.mod libfvcom_cap.a fvcom.mk
	rm -rf $(FVCOM_BINDIR)
	@echo ""
