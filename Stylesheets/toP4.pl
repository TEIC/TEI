use Getopt::Long;
$opt_date="";
$opt_version=0;
$result = GetOptions ("version=s", "date=s", );
while (<>) {
if (/^#include LICENSE/) {
 print "Copyright 1999-2003 Sebastian Rahtz / Text Encoding Initiative Consortium
                          
    This is an XSLT stylesheet for transforming TEI (version P4) XML documents

    Version $opt_version. Date $opt_date

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.
                                                                                
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
                                                                                
    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
                                                                                
    The author may be contacted via the e-mail address

    sebastian.rahtz@computing-services.oxford.ac.uk";
}
else
{ 
     s/tei:teiCorpus/teiCorpus.2/;
     s/tei:TEI/TEI.2/;
     s/TEI.1_/TEI.2.1_/;
     s/tei://g;
     print;
}
}
