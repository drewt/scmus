#
# rules.mk: build rules
#
#  For automatic dependency resolution, set $(objects) to the list of object
#  files needed for the build before including this file.
#
#  $(clean) and $(distclean) should be set to the files to be removed on
#  'make clean' and 'make distclean', respectively.
#
#  $(verbose) controls whether build commands are echoed verbatim, or in
#  prettier "  PROG    FILE" format.  Set verbose=y for verbatim output.
#

.SUFFIXES:
.PHONY: clean distclean run

#dependencies = $(addprefix ., $(addsuffix .d, $(basename $(objects))))
distclean += $(dependencies)

ifeq ($(verbose),y)
  quiet =
else
  quiet = quiet_
endif

clean:
	$(if $(clean), rm -f $(clean))

distclean: clean
	$(if $(distclean), rm -f $(distclean))

run: all
	$(if $(target), ./$(target))

%.o: %.c
	$(call cmd,cc)

%.o: %.scm
	$(call cmd,csc)

%.types: %.scm
	$(call cmd,types)

.%.d: %.c
	$(call cmd,dep)

# CC for program object files (.o)
quiet_cmd_cc    = CC      $@
      cmd_cc    = $(CC) -c $(CPPFLAGS) $(ALLCFLAGS) -o $@ $<

# scheme compile for object files
quiet_cmd_csc   = CSC     $@
      cmd_csc   = $(CSC) -c $(CSCFLAGS) -o $@ $<

quiet_cmd_types = TYPES   $@
      cmd_types = $(CSC) -c $(TFLAGS) -analyze-only -emit-type-file $@ $<

# LD for programs; optional parameter: libraries
quiet_cmd_ld    = LD      $@
      cmd_ld    = $(LD) $(LDFLAGS) -o $@ $^ $(1)

# generate dependencies file
quiet_cmd_dep   = DEP     $@
      cmd_dep   = echo "$@ `$(CC) $(ALLCFLAGS) -MM -I $(incdir) $(CPPFLAGS) $<`" > $@

# call submake
quiet_cmd_smake = MAKE    $@
      cmd_smake = cd $@ && $(MAKE) $(1)

# generate man page as plain text file
quiet_cmd_groff = GROFF   $@
      cmd_groff = groff -man -Tascii $< | col -bx > $@

quiet_cmd_cat   = CAT     $@
      cmd_cat   = cat $^ > $@
# cmd macro (taken from kbuild)
cmd = @$(if $($(quiet)cmd_$(1)),echo '  $(call $(quiet)cmd_$(1),$(2))' &&) $(call cmd_$(1),$(2))

ifneq ($(dependencies),)
-include $(dependencies)
endif
