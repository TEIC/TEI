
<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
  xmlns:fo="http://www.w3.org/1999/XSL/Format"
  indent-result="yes"
  result-ns="">

<xsl:template match="/math">
    <fo:root xmlns:fo="http://www.w3.org/1999/XSL/Format">
      <fo:layout-master-set>
      <fo:simple-page-master
        page-master-reference="right"
        margin-top="75pt"
        margin-bottom="125pt"
        margin-left="100pt"
        margin-right="50pt">
        <fo:region-body margin-bottom="50pt"/>
        <fo:region-after extent="25pt"/>
      </fo:simple-page-master>
      <fo:simple-page-master
        page-master-reference="left"
        margin-top="75pt"
        margin-bottom="125pt"
        margin-left="50pt"
        margin-right="100pt">
        <fo:region-body margin-bottom="50pt"/>
        <fo:region-after extent="25pt"/>
      </fo:simple-page-master>
      </fo:layout-master-set>

      <fo:page-sequence>

        <fo:sequence-specification>
          <fo:sequence-specifier-alternating
            page-master-first="right"
            page-master-odd="right"
            page-master-even="left"/>
        </fo:sequence-specification>

        <fo:static-content flow-name="xsl-after">
          <fo:block text-align-last="centered" font-size="10pt"><fo:page-number/></fo:block>
        </fo:static-content>

        <fo:flow>
          <xsl:apply-templates/>
        </fo:flow>
      </fo:page-sequence>

    </fo:root>
</xsl:template>

<xsl:import href="mathml.xsl"/>

</xsl:stylesheet>
