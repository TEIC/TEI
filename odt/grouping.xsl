<xsl:stylesheet
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 xmlns:xs="http://www.w3.org/2001/XMLSchema"
 xmlns:mf="http://example.com/mf"
 exclude-result-prefixes="xs mf"
 version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet dealing with elements from the core module. </p>
         <p>This software is dual-licensed:

1. Distributed under a Creative Commons Attribution-ShareAlike 3.0
Unported License http://creativecommons.org/licenses/by-sa/3.0/ 

2. http://www.opensource.org/licenses/BSD-2-Clause
		


Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

* Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

This software is provided by the copyright holders and contributors
"as is" and any express or implied warranties, including, but not
limited to, the implied warranties of merchantability and fitness for
a particular purpose are disclaimed. In no event shall the copyright
holder or contributors be liable for any direct, indirect, incidental,
special, exemplary, or consequential damages (including, but not
limited to, procurement of substitute goods or services; loss of use,
data, or profits; or business interruption) however caused and on any
theory of liability, whether in contract, strict liability, or tort
(including negligence or otherwise) arising in any way out of the use
of this software, even if advised of the possibility of such damage.
</p>
         <p>Author: See AUTHORS</p>
         
         <p>Copyright: 2013, TEI Consortium</p>
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

