<?xml version="1.0"?>
<xsl:stylesheet
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    version="1.0"
>

<xsl:output method="html"/>

<xsl:template match="bt">
<tr>
    <xsl:if test="position() mod 2">
        <xsl:attribute name="bgcolor">#eeeeee</xsl:attribute>
    </xsl:if>
    <td>
        <xsl:value-of select="@level"/>
    </td>
    <xsl:apply-templates/>
</tr>
</xsl:template>

<xsl:template match="file|line">
    <td>
        <xsl:apply-templates/>
    </td>
</xsl:template>

<xsl:template match="/">
<html>
<head>
<title>Server Error</title>
<style type="text/css">
h2, h3, h4, p, i, td, th
   	{
		font-family: Verdana, Helvetica, sans-serif;
	}
      th
        {
          color: white;
        }
    </style>
</head>
<body bgcolor="white">
<h2>Server Error</h2>
<p>
The following error occurred: <xsl:value-of select="/error/msg"/>
</p>

<h3>Stack Trace:</h3>
<table border="0" cellpadding="3" cellspacing="0">

<tr bgcolor="blue"><th>Level</th><th>File</th><th>Line #</th></tr>

    <xsl:apply-templates select="/error/stack_trace/*"/>

</table>
</body>
</html>

</xsl:template>

</xsl:stylesheet>
