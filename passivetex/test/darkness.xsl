<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
  xmlns:fo="http://www.w3.org/1999/XSL/Format">

  <xsl:template match="novel">
    <fo:root xmlns:fo="http://www.w3.org/1999/XSL/Format">
      <fo:layout-master-set>
      <fo:simple-page-master
        master-name="simple"
        margin-top="75pt"
        margin-bottom="25pt"
        margin-left="100pt"
        margin-right="50pt">
        <fo:region-body margin-bottom="50pt"/>
        <fo:region-after extent="25pt"/>
      </fo:simple-page-master>
      <fo:page-sequence-master master-name="twoside">
         <fo:repeatable-page-master-reference master-reference="simple"/>
      </fo:page-sequence-master>
      </fo:layout-master-set>

      <fo:page-sequence master-reference="twoside">
        <fo:static-content flow-name="xsl-region-after">
          <fo:block text-align="center" font-size="10pt"><fo:page-number/></fo:block>
        </fo:static-content>

        <fo:flow>
          <xsl:apply-templates/>
        </fo:flow>
      </fo:page-sequence>

    </fo:root>
  </xsl:template>

  <xsl:template match="front/title">
    <fo:block font-size="36pt" text-align="center"><xsl:apply-templates/></fo:block>
  </xsl:template>

  <xsl:template match="author">
    <fo:block font-size="24pt" text-align="center"><xsl:apply-templates/></fo:block>
  </xsl:template>

  <xsl:template match="revision-list">
  </xsl:template>

  <xsl:template match="chapter">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="chapter/title">
    <fo:block font-size="24pt" text-align="center" space-before.optimum="24pt"><xsl:apply-templates/></fo:block>
  </xsl:template>

  <xsl:template match="paragraph">
    <fo:block font-size="12pt" space-before.optimum="12pt" text-align="justify"><xsl:apply-templates/></fo:block>
  </xsl:template>
</xsl:stylesheet>
