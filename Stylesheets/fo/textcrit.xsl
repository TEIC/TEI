<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
                xmlns:fotex="http://www.tug.org/fotex"
                xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                xmlns="http://www.w3.org/1999/XSL/Format"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="a fotex rng tei teix"
                version="2.0">

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet dealing with elements from the textcrit
      module, making FO output. </p>
         <p> This library is free software; you can redistribute it and/or
      modify it under the terms of the GNU Lesser General Public License as
      published by the Free Software Foundation; either version 2.1 of the
      License, or (at your option) any later version. This library is
      distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
      without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
      PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
      details. You should have received a copy of the GNU Lesser General Public
      License along with this library; if not, write to the Free Software
      Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA xs </p>
         <p>Author: See AUTHORS</p>
         <p>Id: $Id$</p>
         <p>Copyright: 2011, TEI Consortium</p>
      </desc>
   </doc>

   <xsl:template name="appReading">
     <xsl:param name="lemma"/>
     <xsl:param name="lemmawitness"/>
     <xsl:param name="readings"/>
     <footnote>
       <footnote-citation>
	 <inline font-size="8pt" vertical-align="super">
	 </inline>
       </footnote-citation>
       <list-block>
	 <xsl:attribute name="provisional-distance-between-starts">
	   <xsl:value-of select="$betweenStarts"/>
	 </xsl:attribute>
	 <xsl:attribute name="provisional-label-separation">
	   <xsl:value-of select="$labelSeparation"/>
	 </xsl:attribute>
	 <list-item>
	   <list-item-label end-indent="label-end()">
	     <block>
	       <inline font-size="{$footnoteSize}" vertical-align="super">
	       </inline>
	     </block>
	   </list-item-label>
	   <list-item-body start-indent="body-start()">
	     <block font-size="{$footnoteSize}">
	       <xsl:copy-of select="$readings"/>
	     </block>
	   </list-item-body>
	 </list-item>
       </list-block>
     </footnote>

  </xsl:template>


</xsl:stylesheet>