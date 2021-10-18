FC=gfortran
#FFLAGS=-g -std=f2008ts -fimplicit-none -static-libgfortran -flto -fopenmp
FFLAGS=-std=f2008ts -fimplicit-none -static-libgfortran -flto -fopenmp
#FOPT=-O3 -ftree-vectorize -fopt-info-vec
FOPT=-O2 -ftree-vectorize -fopt-info-vec

build:
	mkdir -p ./bin ./obj
	$(FC) $(FFLAGS) -c ./src/environment.f90 -J ./obj/ -o ./obj/environment.o
	$(FC) $(FFLAGS) -c ./src/io.f90 -J ./obj/ -o ./obj/io.o
	$(FC) $(FFLAGS) $(FOPT) -c ./src/main.f90 -I ./obj/ -o ./obj/main.o
	$(FC) $(FFLAGS) $(FOPT) ./obj/environment.o ./obj/io.o ./obj/main.o -o ./bin/app

clean:
	rm -Rf ./obj
	rm -Rf ./bin

run:
	./bin/app
