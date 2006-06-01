GNATMAKE ?= gnatmake
GNATCFLAGS = -I../areadline -g -O2 -gnatwa -gnatwe -gnaty

PROGRAMS = test_aforth

all:: $(PROGRAMS)

install:: $(PROGRAMS)
	rsync $(PROGRAMS) /home/shix

test_aforth: never
	$(GNATMAKE) $(GNATCFLAGS) test_aforth

clean:: never
	$(RM) *.o *.ali *~ b~*.ad? $(PROGRAMS)

never::
