GNATMAKE ?= gnatmake
GNATCFLAGS = -I../areadline -g -O2 -gnatwa -gnatwe -gnaty -gnata
PYTHON ?= python

PROGRAMS = test_aforth

all:: $(PROGRAMS)

install:: $(PROGRAMS)
	rsync $(PROGRAMS) /home/shix

%.ads %.adb: %.fs
	$(PYTHON) embed.py $<

test_aforth: never builtins.ads builtins.adb
	$(GNATMAKE) $(GNATCFLAGS) test_aforth

clean:: never
	$(RM) *.o *.ali *~ b~*.ad? $(PROGRAMS)

never::
