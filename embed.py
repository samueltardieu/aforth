#! /usr/bin/python
#
# Usage: embed input_forth_file output_ada_unit
#

import sys
try:
    from functools import reduce  # Python 3 needs this
except:
    pass

try: ada = sys.argv[2]
except IndexError: ada = sys.argv[1][:-3].capitalize()

adafile = ada.lower().replace('.', '-')

outspec = open("%s.ads" % adafile, "w")
outspec.write('''------------------------------------------------------------------------------
--                                                                          --
--                            AFORTH COMPONENTS                             --
--                                                                          --
--                       F O R T H . B U I L T I N S                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2006-2010 Samuel Tardieu <sam@rfc1149.net>        --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--   The main repository for this software is located at:                   --
--       http://git.rfc1149.net/aforth.git                                  --
--                                                                          --
------------------------------------------------------------------------------

--  This file is autogenerated. Changes must be made to the builtins.fs
--  file instead or they will be lost.

private package %s is

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

