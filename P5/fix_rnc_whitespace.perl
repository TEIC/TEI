#!/usr/bin/env perl
#
# $Header$
#
# Copyright 2005 Syd Bauman and Text Encoding Initiative Consortium.
# Some rights reserved. For complete copyleft notice, see block
# comment at the end of this file.
# 
# fix-rnc-whitespace.perl
#
# usage
# -----
# 	$PROGRAM_NAME
# 
# where:
# - input, always from STDIN, should be a Roma-generated RelaxNG
#          compact syntax file.
# - output, always to STDOUT, will be the same file with much prettier
#           use of whitespace.
# 
# Chnage log (CVS-maintained) near the bottom of this file (I hope :-).
#

use English;

# --------- program goes here --------- #

@file = <STDIN>;
$file = join("", @file );
$file =~ s/(.* =\n)  \n/\n\1/g;
$file =~ s/\n(\n[a-zA-Z0-9-]+\.[a-zA-Z0-9.-]+ =\n  ##)/\1/g;
print STDOUT $file;

exit 0;

# -----------------------------------------------------
# Update Hx
# ------ --
# $Log$
# Revision 1.1  2005/04/22 23:45:50  sbauman
# Initial check-in of
# * routine to improve distribution of blank lines in the
#   RelaxNG compact syntax files in Schema/ for human read-
#   ability, and
# * changes to Makefile that use it
#
#
# -----------------------------------------------------
# Copyright 2005 Syd Bauman and Text Encoding Initiative Consortium.
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version. This program is distributed in
# the hope that it will be useful, but WITHOUT ANY WARRANTY; without
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE. See the GNU General Public License for more
# details. You should have received a copy of the GNU General Public
# License along with this program; if not, write to the
#        Free Software Foundation, Inc.
#        675 Mass Ave
#        Cambridge, MA  02139
#        USA
#        gnu@prep.ai.mit.edu
#
# Syd Bauman, senior xml textbase programmer/analyst
# Brown University Women Writers Project
# Box 1841
# Providence, RI  02912-1841
# 401-863-3835
# Syd_Bauman@Brown.edu
#
