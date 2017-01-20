# The compiler
FC = gfortran

# List of executables to be built within the package
PROGRAMS = calday2jday jday2calday

# "make" builds all
all : $(PROGRAMS) 

clean:
	rm -f $(PROGRAMS)

jday2calday: jday2calday.f90
	$(FC) -o $@ jday2calday.f90

calday2jday: calday2jday.f90
	$(FC) -o $@ calday2jday.f90

