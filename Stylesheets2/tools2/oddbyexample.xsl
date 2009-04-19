<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  version="2.0"
>
<!-- This library is free software; you can redistribute it and/or
      modify it under the terms of the GNU Lesser General Public License as
      published by the Free Software Foundation; either version 2.1 of the
      License, or (at your option) any later version. This library is
      distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
      without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
      PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
      details. You should have received a copy of the GNU Lesser General Public
      License along with this library; if not, write to the Free Software
      Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
      02111-1307 USA 

$Id: iden.xsl 4549 2008-04-23 16:40:01Z rahtz $

2008, TEI Consortium
-->

<!-- 
read a corpus of TEI P5 documents and construct
an ODD customization file which expresses the subset
of the TEI you need to validate that corpus
-->

<!-- the document corpus -->
<xsl:param name="corpus">./</xsl:param>
<!-- the source of the TEI (just needs *Spec)-->
<xsl:param name="tei">/usr/share/xml/tei/odd/p5subset.xml</xsl:param>


<!--
1) start a variable and copy in all of the TEI 
2) read the corpus and get a list of all the elements and their
attributes that it uses
3) process the variable and read the TEI section. if an element or
 attribute is not present in the corpus section, put out a delete
 customization
-->
<xsl:template match="/">
</xsl:template>

</xsl:stylesheet>
