#! /usr/bin/python
#
# Usage: embed input_forth_file output_ada_name
#

import sys

try: ada = sys.argv[2]
except IndexError: ada = sys.argv[1][:-3].capitalize()

lower = ada.lower()
open("%s.ads" % lower, "w").write("""package %s is
   pragma Elaborate_Body;
end %s;
""" % (ada, ada))

outbody = open("%s.adb" % lower, "w")
outbody.write("""with Aforth;
pragma Elaborate_All (Aforth);
package body %s is
begin
""" % ada)

for l in open(sys.argv[1]):
    while l[-1:] in ["\r", "\n"]: l = l[:-1]
    if not l: continue
    outbody.write ('   Aforth.Interpret_Line ("%s");\n' %
                   l.replace('"', '""'))

outbody.write("end %s;\n" % ada)

