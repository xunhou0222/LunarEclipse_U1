LIB_TAR = libtesteph.a
LIB_PATH = ..
FC = gfortran-11

.SILENT : 


OBJ = eph.o split.o

$(LIB_TAR) : $(OBJ)
	ar ru $@ $^
	-@echo ""
	-@echo "Library testeph has been bulit successfully!"
	-@echo ""


install : 
	-mkdir -p $(LIB_PATH)
	-mv $(LIB_TAR) $(LIB_PATH)
	-@echo ""
	-@echo "Library testeph has been installed successfully!"
	-@echo ""


.PHONY : clean cleanall
clean :
	rm -f *.o
	-@echo ""
	-@echo "All object files associated with libtesteph.a have been deleted!"
	-@echo ""
cleanall :
	rm -f *.o $(LIB_PATH)/$(LIB_TAR)
	-@echo ""
	-@echo "All object files associated with libtesteph.a and the library iteself have been deleted!"
	-@echo ""