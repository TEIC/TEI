<!DOCTYPE stylesheet []>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">


<xsl:import href="teihtml-teic.xsl"/>
<xsl:variable name="masterFile">teixsl</xsl:variable>

<xsl:template match="tei:xptr[@type='interpret']">
 <xsl:for-each select="document(@url)//body/div">
    <h3><xsl:value-of select="head"/></h3>
    <table  rules="all" border="1">
<tr>
<td><b>Type</b></td><td><b>Name</b></td>
<td><b>Default</b></td><td><b>Comment</b></td>
</tr>
      <xsl:for-each select=".//var">
        <tr>
<td><xsl:value-of select="@method"/></td>
<td><em><xsl:value-of select="@name"/></em></td>
<td><xsl:value-of select="default"/>
<xsl:if test="default = ''">&#160;</xsl:if>
</td>
<td><xsl:value-of select="description"/></td>
        </tr>
       </xsl:for-each>
    </table>
   </xsl:for-each>
 <xsl:for-each select="document(@url)//back/div">
    <xsl:apply-templates/>
   </xsl:for-each>
</xsl:template>

<!-- -->
</xsl:stylesheet>

