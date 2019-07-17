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

   <xsl:template name="makeAppEntry">
     <xsl:param name="lemma"/>
     <footnote>
       <footnote-citation>
	 <inline font-size="8pt" vertical-align="super">
	     <xsl:call-template name="appN"/>
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
		 <xsl:call-template name="appN"/>
	       </inline>
	     </block>
	   </list-item-label>
	   <list-item-body start-indent="body-start()">
	     <block font-size="{$footnoteSize}">
	       <xsl:value-of select="$lemma"/>
	       <xsl:text>] </xsl:text>
	       <xsl:call-template name="appLemmaWitness"/>
	       <xsl:call-template name="appReadings"/>
	     </block>
	   </list-item-body>
	 </list-item>
       </list-block>
     </footnote>

  </xsl:template>


</xsl:stylesheet>
