<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.w3.org/1999/xhtml"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="tei"
                version="2.0">
  
    <!-- import base conversion style -->

    <xsl:import href="../../../epub/tei-to-epub.xsl"/>
    <xsl:param name="cssFile">../profiles/podcasts/epub/podcast.css</xsl:param>
    <xsl:param name="subject">Oxford Podcast</xsl:param>
    
  <xsl:template name="stdheader">
    <xsl:param name="title" select="'(no title)'"/>
    <xsl:variable 
	name="link"
	select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:idno[@type='link']"/>

    <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:publisher/tei:graphic"/>

    <div class="transcriptSummary">
    <p><i><xsl:call-template name="generateTitle"/></i></p>
    <p>Presenter(s): <xsl:sequence select="tei:generateAuthor(.)"/></p>
    <p>A recording is available online here: <a>
	<xsl:attribute name="href">
	  <xsl:value-of select="substring-before($link,'?')"/>
	  <xsl:text>?CAMEFROM=transcript</xsl:text>
	</xsl:attribute>
      <xsl:value-of select="substring-before($link,'?')"/></a></p>
      <p>Part of a series: 
      <i><xsl:value-of select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:sourceDesc/tei:p"/></i></p>
    </div>
<!--      <tr><td class="label"><strong>Description</strong></td><td> <xsl:value-of select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:notesStmt/tei:note"/></td></tr> -->
<!--      <tr><td class="label"><strong>Keywords</strong></td><td> <xsl:for-each  
      select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:profileDesc/tei:textClass/tei:keywords/tei:list/tei:item"> 
      <xsl:value-of select="."/>
      <xsl:if test="following-sibling::tei:item">, </xsl:if>
      </xsl:for-each></td></tr> 
-->


  </xsl:template>

  <xsl:template name="stdfooter">
    <hr/>
    <blockquote>
      <p>
	© <xsl:value-of select="format-dateTime(current-dateTime(),'[Y]')"/>
	University of Oxford, <xsl:sequence select="tei:generateAuthor(.)"/>
      </p>
      
      <p>This transcript is released under the Creative Commons
      Attribution-Non-Commercial-Share Alike 2.0 UK: England &amp; Wales
      Licence. It can be reused and redistributed globally provided that it
      is used in a non-commercial way and the work is attributed to the
      licensors. If a person creates a new work based on the transcript, the
      new work must be distributed under the same licence. Before reusing,
      adapting or redistributing, please read and comply with the full
      licence available at
      <a href="http://creativecommons.org/licenses/by-nc-sa/2.0/uk/">http://creativecommons.org/licenses/by-nc-sa/2.0/uk/</a>
      </p>
    </blockquote>
  </xsl:template>

</xsl:stylesheet>