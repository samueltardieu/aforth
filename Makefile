GNATMAKE ?= gnatmake
GNATCFLAGS = -aP../areadline
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
	$(GNATMAKE) $(GNATCFLAGS) -Paforth

clean:: never
	$(RM) *.o *.ali *~ b~*.ad? $(PROGRAMS) builtins.ads builtins.adb

never::
