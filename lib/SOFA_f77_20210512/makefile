# This software is compilable only by ANSI Fortran 77 compilers - give
# the name of your preferred Fortran compiler (FC) and compilation flags

FC = gfortran-11
FFLAGS = -c -O -Wall

.SILENT : 

#----YOU SHOULDN'T HAVE TO MODIFY ANYTHING BELOW THIS LINE---------

# The list of installation directories.

INSTALL_DIRS = ../

# Name the SOFA/F library in its source and target locations.

SOFA_LIB_NAME = libsofa_20210512.a
SOFA_LIB = $(INSTALL_DIRS)$(SOFA_LIB_NAME)

# The list of SOFA library object files.

SOFA_OBS := $(patsubst %.for, iau_%.o, $(wildcard *.for))

#-----------------------------------------------------------------------
#
#  TARGETS
#
#-----------------------------------------------------------------------

# Build the library.
$(SOFA_LIB_NAME) : $(SOFA_OBS)
	ar ru $(SOFA_LIB_NAME) $?
	-@echo ""
	-@echo "SOFA library has been built successfully!"
	-@echo ""

# Install the library.
install $(SOFA_LIB) : $(INSTALL_DIRS) $(SOFA_LIB_NAME)
	mv $(SOFA_LIB_NAME) $(INSTALL_DIRS)
	-@echo ""
	-@echo "SOFA library has been installed successfully!"
	-@echo ""

# Create the installation directory if not already present.
$(INSTALL_DIRS):
	mkdir -p $@


# Delete object files.
.PHONY : clean cleanall
clean :
	rm -f $(SOFA_OBS)
	-@echo ""
	-@echo "All object files associated with libsofa_20210512.a have been deleted!"
	-@echo ""

# Delete all generated binaries in the current directory.
cleanall:
	rm -f $(SOFA_LIB) $(SOFA_OBS)
	-@echo ""
	-@echo "All object files associated with libsofa_20210512.a and the library iteself have been deleted!"
	-@echo ""

#-----------------------------------------------------------------------

iau_%.o : %.for
	$(FC) $(FFLAGS) $< -o $@