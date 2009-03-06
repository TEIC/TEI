<xsl:stylesheet 
    exclude-result-prefixes="tei" 
    xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
  xmlns="http://www.w3.org/1999/xhtml"
  version="2.0"
>

<xsl:import href="isoutils.xsl"/>
<xsl:output method="xhtml" encoding="utf-8"/>

<xsl:key name="DIV" match="tei:div" use="@type"/>

<xsl:template match="/tei:TEI">
<html>
  <head><title>Report on ISO document</title>
  <link href="iso.css" rel="stylesheet" type="text/css"/>

  </head>
  <body>
    <h1 class="maintitle">Report on metadata</h1>
    <table>
      <tr>
	<td>Today's date</td>
	<td><xsl:call-template name="whatsTheDate"/></td>
      </tr>
      
      <tr>
	<td>title</td>
	<td><xsl:call-template name="generateTitle"/></td>
      </tr>
      <tr>
	<td>title_introductory_en</td>
	<td><xsl:call-template name="getiso_title_introductory_en"/></td>
      </tr>

      <tr>
	<td>title_main_en</td>
	<td><xsl:call-template name="getiso_title_main_en"/></td>
      </tr>

      <tr>
	<td>title_complementary_en</td>
	<td><xsl:call-template name="getiso_title_complementary_en"/></td>
      </tr>
      <tr>
	<td>title_introductory_fr</td>
	<td><xsl:call-template name="getiso_title_introductory_fr"/></td>
      </tr>

      <tr>
	<td>title_main_fr</td>
	<td><xsl:call-template name="getiso_title_main_fr"/></td>
      </tr>

      <tr>
	<td>title_complementary_fr</td>
	<td><xsl:call-template name="getiso_title_complementary_fr"/></td>
      </tr>
     
      <tr>
	<td>authority</td>
	<td><xsl:call-template name="getiso_authority"/></td>
      </tr>

      <tr>
	<td>committee</td>
	<td><xsl:call-template name="getiso_committee"/></td>
      </tr>
      
      <tr>
	<td>documentNumber</td>
	<td><xsl:call-template name="getiso_documentNumber"/></td>
      </tr>
      
      <tr>
	<td>partNumber</td>
	<td><xsl:call-template name="getiso_partNumber"/></td>
      </tr>
      
      <tr>
	<td>year</td>
	<td><xsl:call-template name="getiso_year"/></td>
      </tr>
      <tr>
	<td>serialNumber</td>
	<td><xsl:call-template name="getiso_serialNumber"/></td>
      </tr>

      <tr>
	<td>draftNumber</td>
	<td><xsl:call-template name="getiso_draftNumber"/></td>
      </tr>

      <tr>
	<td>stage</td>
	<td><xsl:call-template name="getiso_stage"/></td>
      </tr>

      <tr>
	<td>supplNumber</td>
	<td><xsl:call-template name="getiso_supplNumber"/></td>
      </tr>

    </table>

</body>
</html>
</xsl:template>

</xsl:stylesheet>

