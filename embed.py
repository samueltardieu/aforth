#! /usr/bin/python
#
# Usage: embed input_forth_file output_ada_unit
#

import sys

try: ada = sys.argv[2]
except IndexError: ada = sys.argv[1][:-3].capitalize()

adafile = ada.lower().replace('.', '-')

open("%s.ads" % adafile, "w").write("""package %s is
   pragma Elaborate_Body;
end %s;
""" % (ada, ada))

outbody = open("%s.adb" % adafile, "w")
if not adafile.startswith('aforth-'):
    outbody.write("""with Aforth;
pragma Elaborate_All (Aforth);""")

outbody.write("""package body %s is
begin
""" % ada)

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

# Make sure we don't split lines after POSTPONE
text = wrap(open(sys.argv[1]).read().replace('POSTPONE ', 'POSTPONE_'), 48).replace('POSTPONE_', 'POSTPONE ')

for l in text.splitlines():
    outbody.write ('   Aforth.Interpret_Line ("%s");\n' % l.replace('"', '""'))

outbody.write("end %s;\n" % ada)

