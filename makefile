SUBDIRS = lib/testeph lib/SOFA_f77_20210512 src
.PHONY : $(SUBDIRS) clean cleanall

all : $(SUBDIRS)
	-@echo ""
	-@echo "2 libraries (SOFA and testeph) and one binary (EclipseFirst) have been built successfully!"
	-@echo ""

$(SUBDIRS) :
	make -C $@
	make -C $@ install

src : lib/testeph


clean : 
	for dir in $(SUBDIRS); do\
	    make -C $$dir clean; \
	done


cleanall :
	for dir in $(SUBDIRS); do\
	    make -C $$dir cleanall; \
	done