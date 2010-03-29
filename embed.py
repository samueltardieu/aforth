#! /usr/bin/python
#
# Usage: embed input_forth_file output_ada_unit
#

import sys

try: ada = sys.argv[2]
except IndexError: ada = sys.argv[1][:-3].capitalize()

adafile = ada.lower().replace('.', '-')

outspec = open("%s.ads" % adafile, "w")
outspec.write('''package %s is

   pragma Preelaborate;

   type String_Access is access constant String;
   type String_Array is array (Positive range <>) of String_Access;

   Builtins : constant String_Array := (
      ''' % ada)

# Snippet copied from http://code.activestate.com/recipes/148061/
# (PSF license)
def wrap(text, width):
    """
    A word-wrap function that preserves existing line breaks
    and most spaces in the text. Expects that existing line
    breaks are posix newlines (\n).
    """
    return reduce(lambda line, word, width=width: '%s%s%s' %
                  (line,
                   ' \n'[(len(line)-line.rfind('\n')-1
                          + len(word.split('\n',1)[0]
                                ) >= width)],
                   word),
                  text.split(' '))

# Protect and unprotect some words that must not be separated
def protect(text):
    return text.replace('POSTPONE ', 'POSTPONE_').replace('] ', ']_')

def unprotect(text):
    return text.replace('POSTPONE_', 'POSTPONE ').replace(']_', '] ')

# Make sure we don't split lines after POSTPONE
text = unprotect(wrap(protect(open(sys.argv[1]).read()), 40))

outspec.write (',\n      '.join(['''new String'("%s")''' % l.replace('"', '""') for l in text.splitlines()]))

outspec.write(""");

end %s;
""" % ada)

