<xsl:stylesheet version="3.0" 
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:teix="http://www.tei-c.org/ns/Examples" 
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  >

  <!--
    extractegXML.xsl: Part of the TEI P5 validation process written many years
    ago by the amazing Sebastian Rahtz.
    
    Read in TEI P5, write out
    a) a directory (./valid/) full of files, one for each example in
       P5, each of which contains nothing but a copy of that single
       single example
    b) a driver file (normal output of this stylesheet) that refers to
       each of those files.

    (Above is part of original program; feature described below added 2021-09-17
    by Syd Bauman and Martin Holmes in attempt to fix Stylesheets issue 417.)
    
    On the way, check the values of @validUntil and replace them with a date 1
    year in the future, so that a date in the past does not fire an error during
    the subsequent validation stage.
  -->

  <xsl:output
    method="xml" exclude-result-prefixes="#all"
    normalization-form="NFC" encoding="UTF-8"/>
  
  <xsl:mode
    name="copy_egXML_checking_validUntil"
    on-no-match="shallow-copy"
    exclude-result-prefixes="#all" />
  
  <xsl:key name="V" match="teix:egXML[@valid='true' or not(@valid)]" use="1"/>
  <xsl:key name="F" match="teix:egXML[@valid='feasible']" use="1"/>
  <xsl:output omit-xml-declaration="yes"/>
  <xsl:template match="/">
    <xsl:text disable-output-escaping="yes">&lt;!DOCTYPE p [</xsl:text>
    <xsl:for-each select="key('V',1)">
      <xsl:variable name="N">
        <xsl:call-template name="loc"/>
      </xsl:variable>
      <xsl:text disable-output-escaping="yes">&lt;!ENTITY </xsl:text>
      <xsl:value-of select="$N"/>
      <xsl:text> SYSTEM "valid/</xsl:text>
      <xsl:value-of select="$N"/>
      <xsl:text disable-output-escaping="yes">"&gt;&#10;</xsl:text>
    </xsl:for-each>
    <xsl:text disable-output-escaping="yes">]&gt;</xsl:text>
    <TEI xmlns="http://www.tei-c.org/ns/1.0">
      <teiHeader>
        <fileDesc>
          <titleStmt>
            <title>The title</title>
          </titleStmt>
          <editionStmt>
            <p/>
          </editionStmt>
          <publicationStmt>
            <p/>
          </publicationStmt>
          <sourceDesc>
            <p/>
          </sourceDesc>
        </fileDesc>
      </teiHeader>
      <text>
        <body>
          <p>
            <xsl:for-each select="key('V',1)">
              <xsl:variable name="N">
                <xsl:call-template name="loc"/>
              </xsl:variable>
              <xsl:result-document href="valid/{$N}">
                <xsl:apply-templates select="." mode="copy_egXML_checking_validUntil"/>
              </xsl:result-document>
              <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
              <xsl:value-of select="$N"/>
              <xsl:text>;</xsl:text>
            </xsl:for-each>
          </p>
        </body>
      </text>
    </TEI>
  </xsl:template>
  
  <xsl:template name="loc">
    <xsl:for-each select="ancestor::tei:*|ancestor-or-self::teix:*">
      <xsl:value-of select="name(.)"/>
      <xsl:text>_</xsl:text>
      <xsl:choose>
        <xsl:when test="@ident">
          <xsl:text></xsl:text>
          <xsl:value-of select="replace(@ident,':','')"/>
          <xsl:text></xsl:text>
        </xsl:when>
        <xsl:when test="@xml:id">
          <xsl:text></xsl:text>
          <xsl:value-of select="replace(@xml:id,':','')"/>
          <xsl:text></xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:number/>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:text>-</xsl:text>
    </xsl:for-each>
  </xsl:template>

  <!-- ********* mode "copy_egXML_checking_validUntil" ********* -->
  <!-- 
       Everything is just copied over (see <xsl:mode> above), except
       any @validUntil attribute is
       1) checked to see that its value is, in fact, a valid W3C date;
       2) converted to a date 1 year in the future from today.

       The point of (2) is to guarentee that the next validation step
       does not complain about the value of a particular @validUntil
       being in the past or too soon or whatever. But that means said
       next validation step will not be testing the actual value of
       @validUntil, but rather our made-up value which will always be
       valid. So we do (1) to test the format of the provided value,
       so the user is told if it is not a valid value.
       
       NOTE: This means that if the datatype of @validUntil is ever
       changed in the TEI _Guidelines_ the test here would have to
       change accordingly. (I.e. we are testing @validUntil against
       xsd:date here not because we happen to love xsd:dates, but
       rather because @validUntil is defined as an xsd:date in
       P5/Source/Specs/att.deprecated.xml)
  -->
  <xsl:template match="@validUntil" mode="copy_egXML_checking_validUntil">
    <!--
      First, knock off leading & trailing spaces, which are allowed in the
      attribute value but may not be in the functions we're calling.
    -->
    <xsl:variable name="validUntil_date" select="normalize-space(.)"/>
    <!--
      Check to see if my value is, in fact, an xsd:date; if not, fire
      off an error message. That is, do this particular validation test
      now, instead of at the next (validation) stage.
    -->
    <xsl:if test="not( $validUntil_date castable as xs:date )">
      <xsl:message>ERROR: the value of this @validUntil (<xsl:value-of
        select="$validUntil_date"/>) is not a W3C date.</xsl:message>
    </xsl:if>
    <!--
      Spit out a dummy @validUntil whose value we know is valid. Thus the
      validation stage will always think the value is valid, it will only
      be checking if this attribute is, in fact, allowed at this spot.
    -->
    <xsl:attribute name="validUntil"
      select="format-date( current-date() + xs:yearMonthDuration('P1Y'),'[Y0001]-[M01]-[D01]')"/>
  </xsl:template>
  
</xsl:stylesheet>
