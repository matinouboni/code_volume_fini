# Compilateur Fortran
FC = gfortran

# Options de compilation
FFLAGS = -Wall -O2

# Liste des fichiers sources
SRCS = main.f90 mod_precision.f90 mod_maillage.f90 mod_sortie.f90 

# Nom de l'exécutable
TARGET = mon_programme

# Règle de compilation par défaut
all: $(TARGET)

# Compilation des fichiers objets
$(TARGET): $(SRCS:.f90=.o)
	$(FC) $(FFLAGS) -o $@ $^

# Règle générique pour la compilation des fichiers source en fichiers objets
%.o: %.f90
	$(FC) $(FFLAGS) -c -o $@ $<

# Nettoyer les fichiers objets et l'exécutable
clean:
	rm -f *.o $(TARGET)
