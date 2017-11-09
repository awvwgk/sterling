PROG=sterling

###------------------------------------------------------------------------###

OBJS = main.o

SRCS=$(patsubst %.F, %.o, $(wildcard *.F))
###------------------------------------------------------------------------###
FC=ifort
CC=icc

# compile flags
FCFLAGS = -O -axAVX -qopenmp -g -traceback -check bounds
CCFLAGS = -O -g -DLINUX -DEBUG
# link flags
FL = ifort -static -fopenmp 
LIBS = 

###------------------------------------------------------------------------###
.PHONY: all
.PHONY: clean

all: $(PROG)

###------------------------------------------------------------------------###
%.o: %.f
	$(FC) $(FCFLAGS) -o $@ -c $<

%.o: %.f90
	$(FC) $(FCFLAGS) -o $@ -c $<

###------------------------------------------------------------------------###
$(PROG): $(OBJS)
	$(FL) $(LIBS) -o $@ $^

clean:
	rm *.o 
	rm *.mod 
	rm $(PROG)
