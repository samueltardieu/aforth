GNATMAKE ?= gnatmake
GNATCFLAGS = -aP../areadline
PYTHON ?= python

PROGRAMS = test_aforth

all:: $(PROGRAMS)

install:: $(PROGRAMS)
	rsync $(PROGRAMS) /home/shix

%.ads %.adb: %.fs
	$(PYTHON) embed.py $<

forth_builtins.ads: builtins.fs embed.py
	$(PYTHON) embed.py $< Forth_Builtins

test_aforth: never forth_builtins.ads
	$(GNATMAKE) $(GNATCFLAGS) -Paforth

clean:: never
	$(RM) *.o *.ali *~ b~*.ad? $(PROGRAMS) \
	  forth_builtins.ads forth_builtins.adb

never::

check-syntax::
	gcc -I../areadline -S -o /dev/null -gnatwa -gnaty $(CHK_SOURCES) 2>&1 | \
	grep -v 'file name does not match unit name' >&2 || true

check: all
	@$(MAKE) -C t check
