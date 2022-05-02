FFLAGS = -L../lib -lsofa_20210512 -ltesteph
EXE_PATH = ../bin
FC = gfortran-11    # Modify this to your own fortran compiler
EXE_TAR = EclipseFirst
    	
.SILENT : install clean cleanall


OBJ = main.o jd2CST.o rho_e2O.o StateVector.o utc2tdb.o

$(EXE_TAR) : $(OBJ)	   
	$(FC) $^ -o $@ $(FFLAGS)
	-@echo ""
	-@echo "$(EXE_TAR) has been bulit successfully!"
	-@echo ""



install :
	mkdir -p $(EXE_PATH)
	mv $(EXE_TAR) $(EXE_PATH)
	-echo ""
	-echo "$(EXE_TAR) has been installed to $(EXE_PATH) successfully!"
	-echo ""


.PHONY : clean cleanall install
clean :
	rm -f *.o
	-echo ""
	-echo "All object files associated with $(EXE_TAR) have been deleted!"
	-echo ""
cleanall :
	rm -f  *.o $(EXE_PATH)/$(EXE_TAR)
	-echo ""
	-echo "All object files associated with $(EXE_TAR) and the binary iteself have been deleted!"
	-echo ""