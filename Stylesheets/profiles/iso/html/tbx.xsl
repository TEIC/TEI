<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.w3.org/1999/xhtml"
		xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
		xmlns:iso="http://www.iso.org/ns/1.0"
		xmlns:cals="http://www.oasis-open.org/specs/tm9901"
		xmlns:html="http://www.w3.org/1999/xhtml"
		xmlns:teix="http://www.tei-c.org/ns/Examples"
		xmlns:s="http://www.ascc.net/xml/schematron"
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:tei="http://www.tei-c.org/ns/1.0"
		xmlns:t="http://www.thaiopensource.com/ns/annotations"
		xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
		xmlns:rng="http://relaxng.org/ns/structure/1.0"
		exclude-result-prefixes="tei html t a rng s iso tbx
					 cals teix" version="2.0">

  <xsl:template match="tbx:term">
    <xsl:if test="../tbx:termNote[@type='administrativeStatus']='deprecatedTerm-admn-sts'">
      <xsl:text>DEPRECATED: </xsl:text>
    </xsl:if>
    <xsl:apply-templates/>
  </xsl:template>

   <xsl:template match="tbx:descrip[@type='definition']">
       <p class="Definition">
	 <xsl:for-each
	     select="ancestor::tbx:termEntry/tbx:descripGrp/tbx:descrip[@type='subjectField']">
	   <span class="domain">
	     <xsl:text>〈</xsl:text>
	     <xsl:value-of select="."/>
	     <xsl:text>〉</xsl:text>
	   </span>
	   <xsl:text> </xsl:text>
	 </xsl:for-each>
	 <xsl:apply-templates/>
	 <xsl:for-each select="../../tbx:admin[@type='source']">
	   <span class="source">
	     <xsl:text> [SOURCE: </xsl:text>
	     <xsl:apply-templates select="."/>
	     <xsl:text>]</xsl:text>
	   </span>
	 </xsl:for-each>
       </p>
   </xsl:template>

  <xsl:template match="tbx:termNote">
    <xsl:choose>
      <xsl:when test="@type='grammaticalGender'">
	<xsl:text>, </xsl:text>
	<span class="gender">
	  <xsl:choose>
	    <xsl:when test=".='masculine'">m</xsl:when>
	    <xsl:when test=".='feminine'">f</xsl:when>
	    <xsl:when test=".='neuter'">n</xsl:when>
	  </xsl:choose>
	</span>
      </xsl:when>
      <xsl:when test="@type='grammaticalNumber'">
	<xsl:text>, </xsl:text>
	<span class="number">
	    <xsl:value-of select="."/>
	</span>
      </xsl:when>
      <xsl:when test="@type='partOfSpeech'">
	<xsl:if test="not(.='noun')">
	  <xsl:text>, </xsl:text>
	  <span class="partOfSpeech">
	    <xsl:value-of select="."/>
	  </span>
	</xsl:if>
      </xsl:when>  
      <xsl:when test="@type='pronunciation'">
	  <xsl:text>, </xsl:text>
	  <span class="pronunciation">
	    <xsl:text>/ </xsl:text>
	    <xsl:value-of select="."/>
	    <xsl:text> /</xsl:text>
	  </span>
      </xsl:when>
      <xsl:when test="@type='geographicalUsage'">
	<xsl:analyze-string select="." regex="^([^\-]+)-([^\-]+)(-x-)?([A-z]*)">
	  <xsl:matching-substring>
	    <xsl:text> </xsl:text>
	    <span class="language">
	      <xsl:value-of select="regex-group(1)"/>
	    </span>
	    <xsl:text> </xsl:text>
	    <span class="geographicalUse">
	      <xsl:value-of select="regex-group(2)"/>
	    </span>
	    <xsl:if test="not(regex-group(4)='')">
	      <span class="script">
		<xsl:text> </xsl:text>
		<xsl:value-of select="regex-group(4)"/>
	      </span>
	    </xsl:if>
	  </xsl:matching-substring>
	  <xsl:non-matching-substring>
	    <span class="geographicalUse">
	      <xsl:value-of select="."/>
	    </span>
	  </xsl:non-matching-substring>
	</xsl:analyze-string>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="termNum">
    <xsl:value-of select="substring-after(../@id,'_')"/>
    <xsl:text> </xsl:text>
  </xsl:template>

  <xsl:template match="tbx:termEntry">
    <div class="termEntry">
      <xsl:call-template name="showTermEntry"/>
    </div>
  </xsl:template>

</xsl:stylesheet>
