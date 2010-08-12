<xsl:stylesheet
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 xmlns:xs="http://www.w3.org/2001/XMLSchema"
 xmlns:mf="http://example.com/mf"
 exclude-result-prefixes="xs mf"
 version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet dealing with elements from the core module. </p>
         <p> This library is free software; you can redistribute it and/or
      modify it under the terms of the GNU Lesser General Public License as
      published by the Free Software Foundation; either version 2.1 of the
      License, or (at your option) any later version. This library is
      distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
      without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
      PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
      details. You should have received a copy of the GNU Lesser General Public
      License along with this library; if not, write to the Free Software
      Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA </p>
         <p>Author: See AUTHORS</p>
         <p>Id: $Id$</p>
         <p>Copyright: 2008, TEI Consortium</p>
      </desc>
   </doc>


 <xsl:output indent="yes"/>

 <xsl:template match="body">
   <xsl:copy>
     <xsl:sequence select="mf:group(*, 1, 0)"/>
   </xsl:copy>
 </xsl:template>

 <xsl:function name="mf:group" as="node()*">
   <xsl:param name="elements" as="element()*"/>
   <xsl:param name="level" as="xs:integer"/>
   <xsl:param name="last-level" as="xs:integer"/>
   <xsl:for-each-group select="$elements"
group-starting-with="head[@level = $level]">
     <xsl:variable name="head" as="element()?"
select=".[self::head[@level = $level]]"/>
     <xsl:variable name="tail" as="element()*" select="current-group()
except $head"/>
     <xsl:choose>
       <xsl:when test="$head">
         <xsl:sequence select="mf:nested-divs($head, $tail, $level,
$level - $last-level)"/>
       </xsl:when>
       <xsl:otherwise>
           <xsl:choose>
             <xsl:when test="$tail[self::head[@level]]">
               <xsl:sequence select="mf:group($tail, $level + 1, $last-level)"/>
             </xsl:when>
             <xsl:otherwise>
               <xsl:copy-of select="$tail"/>
             </xsl:otherwise>
           </xsl:choose>
       </xsl:otherwise>
     </xsl:choose>
   </xsl:for-each-group>
 </xsl:function>

 <xsl:function name="mf:nested-divs" as="element()*">
   <xsl:param name="head" as="element()?"/>
   <xsl:param name="tail" as="element()*"/>
   <xsl:param name="level" as="xs:integer"/>
   <xsl:param name="i" as="xs:integer"/>
   <xsl:choose>
     <xsl:when test="$i gt 0">
       <div>
         <xsl:sequence select="mf:nested-divs($head, $tail, $level, $i - 1)"/>
       </div>
     </xsl:when>
     <xsl:otherwise>
       <xsl:copy-of select="$head"/>
       <xsl:choose>
         <xsl:when test="$tail[self::head[@level]]">
           <xsl:sequence select="mf:group($tail, $level + 1, $level)"/>
         </xsl:when>
         <xsl:otherwise>
           <xsl:copy-of select="$tail"/>
         </xsl:otherwise>
       </xsl:choose>
     </xsl:otherwise>
   </xsl:choose>
 </xsl:function>

</xsl:stylesheet>

<!--
It has become rather complicated with two functions due to the
requirement to add missing levels.

Your latest suggestion to first normalize the input by adding missing
levels and then to group in a second pass might indeed be an approach
leading to shorter and less complicated code.



       Martin Honnen
       http://msmvps.com/blogs/martin_honnen/
-->

