<!-- 
Text Encoding Initiative Consortium XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL stylesheet to format TEI XML documents to HTML or XSL FO

 
##LICENSE
--> 
<xsl:stylesheet
  xmlns:tei="http://www.tei-c.org/ns/1.0"

  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">


<!-- lists -->


<xsl:template match="tei:list">
<xsl:if test="tei:head">
  <p><em><xsl:apply-templates select="tei:head"/></em></p>
</xsl:if>
<xsl:choose>
 <xsl:when test="@type='catalogue'">
  <p><dl>
    <xsl:for-each select="tei:item">
       <p/>
       <xsl:apply-templates select="."  mode="gloss"/>
    </xsl:for-each>
  </dl></p>
 </xsl:when>
  <xsl:when test="@type='gloss' and @rend='multicol'">
    <xsl:variable name="nitems">
      <xsl:value-of select="count(item)div 2"/>
    </xsl:variable>
    <p><table>
    <tr>
      <td valign="top">
      <dl>
         <xsl:apply-templates mode="gloss" select="tei:item[position()&lt;=$nitems ]"/>
      </dl>
      </td>
      <td  valign="top">
      <dl>
         <xsl:apply-templates mode="gloss" select="tei:item[position() &gt;$nitems]"/>
      </dl>
      </td>
     </tr>
    </table>
    </p>
  </xsl:when>

 <xsl:when test="@type='gloss'">
  <p><dl><xsl:apply-templates mode="gloss" select="tei:item"/></dl></p>
 </xsl:when>
 <xsl:when test="@type='glosstable'">
  <table><xsl:apply-templates mode="glosstable" select="tei:item"/></table>
 </xsl:when>
 <xsl:when test="@type='vallist'">
  <table><xsl:apply-templates mode="glosstable" select="tei:item"/></table>
 </xsl:when>
 <xsl:when test="@type='inline'">
   <xsl:if test="not(item)">None</xsl:if>
  <xsl:apply-templates select="tei:item" mode="inline"/>
 </xsl:when>
 <xsl:when test="@type='runin'">
  <p><xsl:apply-templates select="tei:item" mode="runin"/></p>
 </xsl:when>
 <xsl:when test="@type='unordered'">
  <ul>
  <xsl:choose>
  <xsl:when test="@rend and starts-with(@rend,'class:')">
    <xsl:attribute name="class">
      <xsl:value-of select="substring-after(@rend,'class:')"/>
    </xsl:attribute>
  </xsl:when>
  <xsl:when test="@rend">
    <xsl:attribute name="class"><xsl:value-of select="@rend"/></xsl:attribute>
  </xsl:when>
  </xsl:choose>
  <xsl:apply-templates select="tei:item"/></ul>
 </xsl:when>
 <xsl:when test="@type='bibl'">
  <xsl:apply-templates select="tei:item" mode="bibl"/>
 </xsl:when>
 <xsl:when test="starts-with(@type,'ordered')">
  <ol>
    <xsl:if test="starts-with(@type,'ordered:')">
      <xsl:attribute name="start">
        <xsl:value-of select="substring-after(@type,':')"/>
      </xsl:attribute>
    </xsl:if>
    <xsl:choose>
        <xsl:when test="@rend and starts-with(@rend,'class:')">
    <xsl:attribute name="class">
      <xsl:value-of select="substring-after(@rend,'class:')"/>
    </xsl:attribute>
  </xsl:when>
  <xsl:when test="@rend">
    <xsl:attribute name="class"><xsl:value-of select="@rend"/></xsl:attribute>
  </xsl:when>
</xsl:choose>
  <xsl:apply-templates select="tei:item"/></ol>
 </xsl:when>
 <xsl:otherwise>
  <ul>
    <xsl:choose>
        <xsl:when test="@rend and starts-with(@rend,'class:')">
    <xsl:attribute name="class">
      <xsl:value-of select="substring-after(@rend,'class:')"/>
    </xsl:attribute>
  </xsl:when>
  <xsl:when test="@rend">
    <xsl:attribute name="class"><xsl:value-of select="@rend"/></xsl:attribute>
  </xsl:when>
</xsl:choose>
  <xsl:apply-templates select="tei:item"/></ul>
 </xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template mode="bibl" match="tei:item">
 <p>
   <xsl:call-template name="makeAnchor"/>
   <xsl:apply-templates/>
 </p>
</xsl:template>

<xsl:template mode="glosstable" match="tei:item">
 <tr>
   <td valign="top"><strong>
     <xsl:apply-templates mode="print" select="preceding-sibling::tei:*[1]"/></strong></td>
   <td><xsl:call-template name="makeAnchor"/><xsl:apply-templates/></td>
 </tr>
</xsl:template>

<xsl:template mode="gloss" match="tei:item">
   <dt><xsl:call-template name="makeAnchor"/><strong>
     <xsl:apply-templates mode="print" select="preceding-sibling::tei:label[1]"/>
   </strong>
   </dt>
   <dd>   <xsl:apply-templates/></dd>
</xsl:template>

<xsl:template match="tei:item/label">
    <xsl:choose>
	<xsl:when test="@rend">
          <xsl:call-template name="rendering"/>
	</xsl:when>
        <xsl:otherwise>
          <strong><xsl:apply-templates/></strong>
        </xsl:otherwise>     
    </xsl:choose>
</xsl:template>

<xsl:template match="tei:list/tei:label"/>

<xsl:template match="tei:item">
  <li>
    <xsl:if test="@rend">
      <xsl:attribute name="class"><xsl:value-of select="@rend"/></xsl:attribute>
    </xsl:if>
    <xsl:if test="@n">
      <xsl:attribute name="value"><xsl:value-of select="@n"/></xsl:attribute>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="@id|@xml:id">
	<a name="{@id|@xml:id}"/>
      </xsl:when>
      <xsl:when test="$generateParagraphIDs='true'">
	<a name="{generate-id()}"/>
      </xsl:when>
    </xsl:choose>
    <xsl:apply-templates/>
  </li>
</xsl:template>

<xsl:template match="tei:item" mode="runin">
  &#8226; <xsl:apply-templates/>&#160;
</xsl:template>

<xsl:template match="tei:item" mode="inline">
  <xsl:if test="preceding-sibling::tei:item">,  </xsl:if>
  <xsl:if test="not(following-sibling::tei:item) and preceding-sibling::tei:item"> and  </xsl:if>   
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:label" mode="print">
  <xsl:if test="@id|@xml:id"><a name="{@id|@xml:id}"/></xsl:if>
  <xsl:choose>
    <xsl:when test="@rend">
      <xsl:call-template name="rendering"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:apply-templates/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="tei:list" mode="inpara">
    <p><xsl:apply-templates select="preceding-sibling::node()"/></p>
    <xsl:apply-templates select="."/>
    <p><xsl:apply-templates select="following-sibling::node()"/></p>
  </xsl:template>
  
  
</xsl:stylesheet>
