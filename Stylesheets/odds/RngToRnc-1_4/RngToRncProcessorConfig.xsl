<?xml version="1.0"?><!--*- XML -*-->

<!-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->
<!-- $Id$ -->
<!-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->

<!-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

     Copyright (c) 2002, Pantor Engineering AB
     All rights reserved.
     
     Redistribution and use in source and binary forms, with or
     without modification, are permitted provided that the following
     conditions are met:
     
     * Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer. 

     * Redistributions in binary form must reproduce the above
       copyright notice, this list of conditions and the following
       disclaimer in the documentation and/or other materials provided
       with the distribution.

     * Neither the name of Pantor Engineering AB nor the names of its
       contributors may be used to endorse or promote products derived
       from this software without specific prior written permission.
     
     THIS   SOFTWARE   IS PROVIDED BY    THE   COPYRIGHT  HOLDERS  AND
     CONTRIBUTORS "AS IS"   AND ANY  EXPRESS OR  IMPLIED   WARRANTIES,
     INCLUDING,  BUT  NOT LIMITED  TO,   THE  IMPLIED  WARRANTIES   OF
     MERCHANTABILITY    AND  FITNESS  FOR   A  PARTICULAR  PURPOSE ARE
     DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
     BE  LIABLE   FOR ANY    DIRECT, INDIRECT,   INCIDENTAL,  SPECIAL,
     EXEMPLARY, OR CONSEQUENTIAL DAMAGES  (INCLUDING, BUT NOT  LIMITED
     TO, PROCUREMENT  OF  SUBSTITUTE GOODS OR  SERVICES;  LOSS OF USE,
     DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
     ANY THEORY OF  LIABILITY, WHETHER IN CONTRACT,  STRICT LIABILITY,
     OR  TORT (INCLUDING NEGLIGENCE OR  OTHERWISE) ARISING  IN ANY WAY
     OUT OF  THE  USE OF   THIS  SOFTWARE,  EVEN IF ADVISED   OF   THE
     POSSIBILITY OF SUCH DAMAGE.

     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->

<!-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     Created by David.Rosenborg@pantor.com
     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->

<!-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

     This stylesheet module contains XSLT processor specific
     instructions. The conversion from the RELAX NG XML syntax to the
     compact syntax, rendered as either HTML or plain text, is
     performed in two steps. To be able to run both steps up on a
     single invokation, an extension function is needed. The function
     should convert a result tree fragment to a node set. The EXSLT
     initiative defines such a function: node-set (). If your
     processor supports this function, this configuration should work
     without any modifications. There are also instructions for the
     Microsoft processor.

     If you add rules for other processors here, I'd be glad to add
     them to the original distribution if you send them in a mail to
     me at David.Rosenborg@pantor.com, thanks!
	
     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->

<xsl:transform
  version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:exslt="http://exslt.org/common"
  xmlns:estr="http://exslt.org/strings"
  extension-element-prefixes="exslt estr"
  xmlns:ms="urn:schemas-microsoft-com:xslt"
>

  <xsl:template name="make-body-from-r-t-f">
    <xsl:param name="schema"/>
    
    <xsl:choose>
      <!-- EXSLT (xsltproc, Saxon et al) -->
      <xsl:when test="function-available ('exslt:node-set')">
	<xsl:for-each select="exslt:node-set ($schema)">
	  <xsl:call-template name="make-body"/>
	</xsl:for-each>
      </xsl:when>

      <!-- Workaround for Xalan (2.4) in which function-available 
      returns false for exslt:node-set even though it does implement it -->
      <xsl:when test="starts-with (system-property ('xsl:vendor'), 'Apache')">
	<xsl:for-each select="exslt:node-set ($schema)">
	  <xsl:call-template name="make-body"/>
	</xsl:for-each>
      </xsl:when>

      <!-- Microsoft -->
      <xsl:when test="function-available ('ms:node-set')">
	<xsl:for-each select="ms:node-set ($schema)">
	  <xsl:call-template name="make-body"/>
	</xsl:for-each>
      </xsl:when>

      <xsl:otherwise>
	<xsl:message terminate="yes">
	  <xsl:text>ERROR: No xx:node-set () function defined for </xsl:text>
	  <xsl:text>this processor. Instructions for a particular </xsl:text>
	  <xsl:text>XSLT processor can be added to the </xsl:text>
	  <xsl:text>RngToRncProcessorConfig.xsl stylesheet module.</xsl:text>
	</xsl:message>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- Pantor specifics, ignored by other processors -->
  
  <pax:extension-function
    name="exslt:node-set"
    implementation="java:com.pantor.pax.extensions.NodeSetFunction"
    xmlns:pax="http://www.pantor.com/pax"
    xmlns:java="http://www.pantor.com/pax/java"
    />

</xsl:transform>
