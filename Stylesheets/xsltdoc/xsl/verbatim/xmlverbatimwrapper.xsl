<?xml version="1.0" encoding="ISO-8859-1"?>

<!--
   XML to HTML Verbatim Formatter with Syntax Highlighting
   HTML wrapper
   Version 1.1
   LGPL (c) Oliver Becker, 2002-11-04
   obecker@informatik.hu-berlin.de
-->

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

   <xsl:import href="xmlverbatim.xsl" />

   <!-- select the name of an element that should be formatted
        (print only these elements and their contents) -->
   <xsl:param name="select" />

   <!-- CSS Stylesheet -->
   

   <xsl:template match="/" mode="xmlverbwrapper">
      <xsl:param name="css-stylesheet" select="'xmlverbatim.css'" />
      <html>
         <head>
            <title>XML source view</title>
            <link rel="stylesheet" type="text/css" 
	          href="{$css-stylesheet}" />
         </head>
         <body class="xmlverb-default">
            <tt>
               <xsl:choose>
                  <!-- "select" parameter present? -->
                  <xsl:when test="$select">
                     <xsl:apply-templates mode="xmlverbwrapper" />
                  </xsl:when>
                  <xsl:otherwise>
                     <xsl:apply-templates select="." mode="xmlverb" />
                  </xsl:otherwise>
               </xsl:choose>
            </tt>
            <br /><br /><br /><br /><br /><br /><br /><br /><br /><br /><br /><br /><br /><br /><br /><br /><br /><br /><br /><br /><br /><br /><br /><br /><br /><br /><br /><br /><br /><br /><br /><br /><br /><br /><br /><br /><br /><br />v
         </body>
      </html>
      <xsl:text>&#xA;</xsl:text>
   </xsl:template>

   <xsl:template match="*" mode="xmlverbwrapper">
      <xsl:choose>
         <xsl:when test="name()=$select">
            <!-- switch to render mode -->
            <!-- print indent -->
            <span class="xmlverb-text">
               <xsl:call-template name="preformatted-output">
                  <xsl:with-param name="text">
                     <xsl:call-template name="find-last-line">
                        <xsl:with-param name="text"
                              select="preceding-sibling::node()[1][self::text()]" />
                     </xsl:call-template>
                  </xsl:with-param>
               </xsl:call-template>
            </span>
            <!-- print element -->
            <xsl:apply-templates select="." mode="xmlverb" />
            <br /><br />
         </xsl:when>
         <xsl:otherwise>
            <!-- look for the selected element among the children -->
            <xsl:apply-templates select="*" mode="xmlverbwrapper" />
         </xsl:otherwise>
      </xsl:choose>
   </xsl:template>

   <!-- return the last line (after newline) in parameter $text -->
   <xsl:template name="find-last-line">
      <xsl:param name="text" />
      <xsl:choose>
         <xsl:when test="contains($text,'&#xA;')">
            <xsl:call-template name="find-last-line">
               <xsl:with-param name="text"
                    select="substring-after($text,'&#xA;')" />
            </xsl:call-template>
         </xsl:when>
         <xsl:otherwise>
            <xsl:value-of select="$text" />
         </xsl:otherwise>
      </xsl:choose>
   </xsl:template>

</xsl:stylesheet>
