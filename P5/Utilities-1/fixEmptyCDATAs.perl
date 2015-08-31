#!/usr/bin/env perl
#
# Copyright 2004 Syd Bauman and the Text Encoding Initiative
# Consortium. Some rights reserved. For complete copyleft notice, see
# block comment at the end of this file.
# 
# fixEmptyCDATAs.perl
#
# usage
# -----
# 	$PROGRAM_NAME
# 
# where:
#   Input is always from STDIN, should be an XML file.
#   Output is always to STDOUT, is the same file with CDATA marked
#          sections that contain nothing but whitespace deleted.
# 
# Chnage log near the bottom of this file.
#

use English;

$INPUT_RECORD_SEPARATOR = "=";
while (<>) {
    s/<!\[CDATA\[(\s+)\]\]>/$1/g;
    print;
}

exit 0;

# -----------------------------------------------------
# Update Hx
# ------ --
# 2004-12-12: Conceived & written.
#
# -----------------------------------------------------
# Copyright 2004 Syd Bauman and the Text Encoding Initiative
# Consortium. This program is free software; you can redistribute it
# and/or modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2 of
# the License, or (at your option) any later version. This program is
# distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
# for more details. You should have received a copy of the GNU General
# Public License along with this program; if not, write to the
#        Free Software Foundation, Inc.
#        675 Mass Ave
#        Cambridge, MA  02139
#        USA
#        gnu@prep.ai.mit.edu
#
# Syd Bauman, xml textbase programmer/analyst
# Brown University Women Writers Project
# Box 1841
# Providence, RI  02912-1841
# 401-863-3835
# Syd_Bauman@Brown.edu
#
