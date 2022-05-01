LIB = -L../lib -lsofa_20210512 -ltesteph
EXE_PATH = ../bin
FC = gfortran-11    # Modify this to your own fortran compiler
EXE_TAR = EclipseFirst
    	

OBJ = main.o jd2CST.o rho_e2O.o StateVector.o utc2tdb.o

$(EXE_PATH)/$(EXE_TAR) : $(OBJ)
	@mkdir -p $(EXE_PATH)	   
	$(FC) $^ -o $@ $(LIB)


.PHONY : clean cleanall
clean :
	-rm *.o
cleanall :
	-rm -rf  *.o $(EXE_PATH)