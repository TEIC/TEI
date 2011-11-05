<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		version="2.0">
<!--This software is dual-licensed:

1. Distributed under a Creative Commons Attribution-ShareAlike 3.0
Unported License http://creativecommons.org/licenses/by-sa/3.0/ 

2. http://www.opensource.org/licenses/BSD-2-Clause
		
All rights reserved.

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

$Id$

2008, TEI Consortium
-->

<xsl:output method="text"/>
   <xsl:key name="TEMPLATES" match="xsl:template[@name]" use="1"/>
   <xsl:key name="CALLED_TEMPLATES" match="xsl:call-template[@name]" use="@name"/>

   <xsl:template match="xsl:stylesheet">
      <xsl:variable name="all">
         <xsl:copy-of select="xsl:template"/>
         <xsl:call-template name="inclusions"/>
      </xsl:variable>
      <xsl:for-each select="$all">
         <xsl:for-each select="key('TEMPLATES',1)">
            <xsl:if test="count(key('CALLED_TEMPLATES',@name))=0">
	Template <xsl:value-of select="@name"/> is not used
      </xsl:if>
         </xsl:for-each>
      </xsl:for-each>
   </xsl:template>

   <xsl:template name="inclusions">
      <xsl:for-each select="xsl:import|xsl:include">
         <xsl:for-each select="document(@href)/xsl:stylesheet">
            <xsl:copy-of select="xsl:template"/>
            <xsl:call-template name="inclusions"/>
         </xsl:for-each>
      </xsl:for-each>
   </xsl:template>
</xsl:stylesheet>