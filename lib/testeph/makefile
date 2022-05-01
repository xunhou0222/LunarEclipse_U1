LIB_TAR = libtesteph.a
LIB_PATH = ..
FC = gfortran-11


all : $(LIB_TAR) install


OBJ = eph.o split.o

$(LIB_TAR) : $(OBJ)
	ar -rc $@ $^


install : 
	mv $(LIB_TAR) $(LIB_PATH)




.PHONY : clean
clean :
	-rm *.o