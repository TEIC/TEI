use Getopt::Long;
$opt_version=0;
$opt_date="";
$result = GetOptions ("version=s", "date=s", );
while (<>) {
if (/^##LICENSE/) {
    print "Copyright 1999-2005 Sebastian Rahtz / Text Encoding Initiative Consortium
                                              
    This is an XSLT stylesheet for transforming TEI (version P5) XML documents

    Version $opt_version. Date $opt_date

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    The author may be contacted via the e-mail address

    sebastian.rahtz\@computing-services.oxford.ac.uk";
}
else
{ 
     print;
}
}
