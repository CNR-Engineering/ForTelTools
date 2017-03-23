#####################################
# Makefile to compile Telemac tools #
#####################################
#
# Usage:
#   * `make` or `make all`: compile main fortran files
#   * `make clean`: remove intermediate files
#   * `make mrproper`: remove all generated files
#
# Info:
#   - Requires gfortran
#   - File organisation:
#	    * ./bin/    main program binaries (compiled files)
#       * ./lib/    subroutines dependencies (are compiled but not linked)
#       * ./mod/    modules dependencies (are compiled but not linked)
#	    * ./src/    main program sources
#
# Warning:
#   for a complete compatibility, compilation with Fortran 2003 standard is required.
#
# Problem/todo:
#   * compilation is not optimal:
#       Everything is compiled with the whole list of `lib ` files (even if they are not used/required)
#       As a workaround, the `strip``tool is used to purge binary files
#   * The use of `stip` is not compatible with code profiling, because symbols are removed for debugging.
#   * Some files (such as .mod files) are invisible for the Makefile but they are built at compilation
#
## COMPILER (/!\ options: -o and -c are used)
### gfortran
FC         = gfortran
FFLAGS_ORI = -ffixed-line-length-132
FFLAGS     = $(FFLAGS_ORI) -Wall
#FFLAGS     = $(FFLAGS_ORI) -g -fbacktrace  #debug (remove set strip to `false`)
STRIP      = true #false

## FILES/FOLDER STRUCTURE
LIB_FOLDER = lib
LIB_OBJ    = $(patsubst %.f, %.o, $(wildcard $(LIB_FOLDER)/*.f))
MOD_FOLDER = mod
MOD_OBJ    = $(MOD_FOLDER)/common_data.o $(patsubst %.f, %.o, $(wildcard $(MOD_FOLDER)/*.f)) #FIXME: common_data.o est dupliqué

BIN_FOLDER = bin
BIN_OBJ    = $(patsubst src/%.f, bin/%, $(wildcard src/*.f))

.PHONY: bin createfolder clean mrproper
.PRECIOUS: %.o

# Create BIN_FOLDER and compile all main programs
bin: createfolder $(BIN_OBJ) clean

createfolder:
	mkdir -p $(BIN_FOLDER)

# Compile and link main program without warnings (src/*_ori.f -> bin)
bin/%_ori: $(LIB_OBJ) src/%_ori.f
	$(FC) $(FFLAGS_ORI) $^ -o $@

# Compile and link main program with warnings (src -> bin)
bin/%: $(LIB_OBJ) $(MOD_OBJ) src/%.f
	$(FC) $(FFLAGS) $^ -o $@
ifeq ($(STRIP),true)
	strip $@
endif

# Compile an individual file (lib)
$(LIB_FOLDER)/%.o: $(LIB_FOLDER)/%.f
	$(FC) $(FFLAGS) -c $^ -o $@

# Compile an individual file (mod)
#FIXME: position of mod files is compiler dependant?
$(MOD_FOLDER)/%.o %.mod: $(LIB_FOLDER)/%.f
	$(FC) $(FFLAGS) -c $^ -o $@


clean :
	find -name '*.o' -delete
	find -name '*.mod' -delete

mrproper: clean
	rm -rf $(BIN_FOLDER)
