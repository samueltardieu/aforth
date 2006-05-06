GNATMAKE ?= gnatmake
GNATCFLAGS = -g -O3 -gnatwa -gnatwe
# -O3 is needed on sh4 because of a bug when compiling png_io-open

PROGRAMS = test_aforth

all:: $(PROGRAMS)

install:: $(PROGRAMS)
	cp $(PROGRAMS) /home/shix

test_aforth: never
	$(GNATMAKE) $(GNATCFLAGS) aforth

clean:: never
	$(RM) *.o *.ali *~ b~*.ad? $(PROGRAMS)

never::
