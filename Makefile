GNATMAKE ?= gnatmake
GNATCFLAGS = -aP../areadline
PYTHON ?= python

PROGRAMS = test_aforth

all:: $(PROGRAMS)

install:: $(PROGRAMS)
	rsync $(PROGRAMS) /home/shix

%.ads %.adb: %.fs
	$(PYTHON) embed.py $<

aforth-builtins.ads aforth-builtins.adb: builtins.fs embed.py
	$(PYTHON) embed.py $< Aforth.Builtins

test_aforth: never aforth-builtins.ads aforth-builtins.adb
	$(GNATMAKE) $(GNATCFLAGS) -Paforth

clean:: never
	$(RM) *.o *.ali *~ b~*.ad? $(PROGRAMS) \
	  aforth-builtins.ads aforth-builtins.adb

never::

check-syntax::
	gcc -I../areadline -S -o /dev/null -gnatwa -gnaty $(CHK_SOURCES) 2>&1 | \
	grep -v 'file name does not match unit name' >&2 || true
