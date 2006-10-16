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

def split(l):
    r = ''
    lines = []
    while len(l) > 40:
        lines.append(l[:40])
        l = l[40:]
    if l: lines.append(l)
    for n, l in enumerate(lines):
        if n: r += ' &\n      '
        r += '"%s"' % l.replace('"', '""')
    return r

for l in open(sys.argv[1]):
    while l[-1:] in ["\r", "\n"]: l = l[:-1]
    if not l: continue
    outbody.write ('   Aforth.Interpret_Line (%s);\n' %
                   split(l))

outbody.write("end %s;\n" % ada)

