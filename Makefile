GNATMAKE ?= gnatmake
GNATCFLAGS = -I../areadline -g -O2 -gnatwa -gnatwe -gnaty -gnata
PYTHON ?= python

PROGRAMS = test_aforth

all:: $(PROGRAMS)

install:: $(PROGRAMS)
	rsync $(PROGRAMS) /home/shix

%.ads %.adb: %.fs
	$(PYTHON) embed.py $<

aforth-builtins.ads aforth-builtins.adb: builtins.fs
	$(PYTHON) embed.py $< Aforth.Builtins

test_aforth: never aforth-builtins.ads aforth-builtins.adb
	$(GNATMAKE) $(GNATCFLAGS) test_aforth

clean:: never
	$(RM) *.o *.ali *~ b~*.ad? $(PROGRAMS) builtins.ads builtins.adb

never::
