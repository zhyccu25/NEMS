########################################################################
#
# Main driver for NOAA R&D computing environment support
#
########################################################################

ifneq (,$(and $(wildcard /scratch4),$(wildcard /scratch3)))
  NEMS_COMPILER?=intel
  $(call add_build_env,theia.$(NEMS_COMPILER),env/rdhpcs/theia.$(NEMS_COMPILER).mk)
else
  ifneq (,$(and $(wildcard /lfs1),$(wildcard /lfs3)))
    NEMS_COMPILER?=intel
    $(call add_build_env,jet.$(NEMS_COMPILER),env/rdhpcs/jet.$(NEMS_COMPILER).mk)
  else
    ifneq (,$(shell hostname | grep -i gaea))
      NEMS_COMPILER?=intel
      $(call add_build_env,gaea.$(NEMS_COMPILER),env/rdhpcs/gaea.$(NEMS_COMPILER).mk)
    endif
  endif
endif
