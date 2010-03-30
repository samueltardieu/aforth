GNATMAKE ?= gnatmake
GNATCFLAGS = -aP../areadline
PYTHON ?= python

PROGRAMS = test_aforth

all:: $(PROGRAMS)

install:: $(PROGRAMS)
	rsync $(PROGRAMS) /home/shix

%.ads %.adb: %.fs
	$(PYTHON) embed.py $<

forth-builtins.ads: builtins.fs embed.py
	$(PYTHON) embed.py $< Forth.Builtins

test_aforth: never forth-builtins.ads
	$(GNATMAKE) $(GNATCFLAGS) -Paforth

clean:: never
	$(RM) *.o *.ali *~ b~*.ad? $(PROGRAMS) \
	  forth-builtins.ads forth-builtins.adb

never::

check-syntax::
	gcc -I../areadline -S -o /dev/null -gnatwa -gnaty $(CHK_SOURCES) 2>&1 | \
	grep -v 'file name does not match unit name' >&2 || true

check: all
	@$(MAKE) -C t check
