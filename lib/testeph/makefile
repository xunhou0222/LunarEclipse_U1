LIB_TAR = libtesteph.a
LIB_PATH = ..
FC = gfortran-11


OBJ = eph.o split.o

$(LIB_TAR) : $(OBJ)
	ar -rc $@ $^
	-@echo ""
	-@echo "Library testeph has been bulit successfully!"
	-@echo ""


install : 
	mkdir -p $(LIB_PATH)
	mv $(LIB_TAR) $(LIB_PATH)
	-@echo ""
	-@echo "Library testeph has been installed successfully!"
	-@echo ""


.PHONY : clean cleanall
clean :
	-rm *.o
cleanall :
	-rm *.o $(LIB_PATH)/$(LIB_TAR)