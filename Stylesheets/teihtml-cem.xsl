<!-- 
TEI XSLT stylesheet family version 3.0
RCS: $Date$, $Revision$, $Author$

XSL stylesheet to format TEI XML documents to FO or HTML

#include LICENSE
-->
<xsl:stylesheet
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
  version="1.0"
  xmlns:exsl="http://exslt.org/common"
  extension-element-prefixes="exsl"
  exclude-result-prefixes="exsl" 
  xmlns:html="http://www.w3.org/1999/xhtml">

  <xsl:import href="teihtml.xsl"/>

   <xsl:param name="cellAlign">center</xsl:param>
   <xsl:param name="autoToc"></xsl:param>
   <xsl:template name="logoPicture"></xsl:template>
   <xsl:param name="cssFile">cem.css</xsl:param>
   <xsl:param name="feedbackURL">pcYears.html</xsl:param>
   <xsl:template name="feedbackWords">Catalogue by Year</xsl:template>
   <xsl:param name="homeLabel">Protestant Cemetery Catalogue</xsl:param>
   <xsl:param name="homeURL">pcCountries.html</xsl:param>
   <xsl:param name="homeWords">Catalogue by Country</xsl:param>
   <xsl:param name="institution">Protestant Cemetery catalogue</xsl:param>
   <xsl:param name="department"/>
   <xsl:param name="parentURL">index.html</xsl:param>
   <xsl:param name="parentWords">Protestant Cemetery, Rome</xsl:param>
   <xsl:param name="searchURL">pcNames.html</xsl:param>
   <xsl:template name="searchWords">Catalogue by Name</xsl:template>
   <xsl:param name="topNavigationPanel"></xsl:param>
   <xsl:param name="bottomNavigationPanel"></xsl:param>
   <xsl:param name="alignNavigationPanel">right</xsl:param>
   <xsl:param name="linkPanel">true</xsl:param>
   <xsl:template name="copyrightStatement">British School at Rome</xsl:template>

<xsl:key name="nats" match="tei:person" use="tei:nationality/@code"/>
<xsl:key name="CATS" match="tei:category" use="@id"/>
<xsl:key name="LANGS" match="tei:language" use="@id"/>

<xsl:template match="tei:teiCorpus">

 <html><xsl:call-template name="addLangAtt"/> 
 <head>
 <title><xsl:apply-templates select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title/text()"/></title>
 <xsl:call-template name="includeCSS"/>
 </head>
 <body>
 <xsl:call-template name="bodyHook"/>
 <xsl:call-template name="bodyJavaScript"/>
 <xsl:call-template name="stdheader">
  <xsl:with-param name="title">
   <xsl:apply-templates select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title"/>
  </xsl:with-param>
 </xsl:call-template>


 <xsl:call-template name="stdfooter">
       <xsl:with-param name="date">
         <xsl:choose>
          <xsl:when test="tei:teiHeader/tei:revisionDesc//tei:date[1]">
            <xsl:value-of select="tei:teiHeader/tei:revisionDesc//tei:date[1]"/>
          </xsl:when>
          <xsl:otherwise>
    	   <xsl:text>(undated)</xsl:text>
          </xsl:otherwise>    
         </xsl:choose>
       </xsl:with-param>
       <xsl:with-param name="author"/>
   </xsl:call-template>
 </body>
 </html>

 <xsl:apply-templates select="tei:TEI"/>

   <xsl:call-template name="sort-by-year"/>
   
   <xsl:call-template name="sort-by-name"/>

   <xsl:call-template name="sort-by-nationality"/>

</xsl:template>

<xsl:template match="tei:TEI">
  <xsl:call-template name="outputChunk">
    <xsl:with-param name="ident" select="@id"/>
    <xsl:with-param name="content">
      <xsl:call-template name="writeDiv"/>          
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template name="doDivBody">
 <xsl:param name="Country" select="'XXX'"/>
 <xsl:variable name="z" select=".//location/zone/@target"/>
 <xsl:variable name="f" select=".//form/@target"/>
 <xsl:variable name="m" select=".//material/@target"/>
 <xsl:variable name="n" select="tei:teiHeader/tei:profileDesc/tei:particDesc/tei:person[1]/tei:nationality/@code"/>
 <xsl:variable name="p1"><xsl:value-of select="tei:teiHeader/tei:profileDesc/tei:particDesc/tei:person[1]/tei:persName/tei:surname"/></xsl:variable>

<table>
<tr valign="top">
<td>
 <xsl:value-of select="@id"/>
</td>
<td>
  <xsl:value-of select="key('CATS',$z)/tei:catDesc"/>
   <xsl:text>;  </xsl:text>
   <xsl:value-of select="key('CATS',$f)/tei:catDesc"/>
   <xsl:text>;  </xsl:text>
   <xsl:value-of select="key('CATS',$m)/tei:catDesc"/>.
<xsl:value-of select=".//height"/> x
   <xsl:value-of select=".//width"/> x <xsl:value-of select=".//breadth"/>
</td>
 <xsl:apply-templates select="tei:teiHeader/tei:profileDesc/tei:particDesc/tei:person[1]"/>
</tr>
<xsl:if test="count(tei:teiHeader/tei:profileDesc/tei:particDesc/tei:person) &gt; 1">
  <xsl:for-each select="tei:teiHeader/tei:profileDesc/tei:particDesc/tei:person">
     <xsl:if test="position() &gt; 1">
      <tr><td/><td/><xsl:apply-templates select="."/></tr>
     </xsl:if>
  </xsl:for-each>
</xsl:if>
</table>

<xsl:if test="count(text/body/div)&gt;0">
 <table>
   <xsl:apply-templates select="tei:text/tei:body/div"/>
 </table>
</xsl:if>
<xsl:apply-templates select=".//physicalDescription/decoration"/>
</xsl:template>


<xsl:template match="tei:person">
   <td><xsl:apply-templates select="tei:persName"/></td>
   <td>
      * <xsl:apply-templates select="tei:birth"/>
      <xsl:text> </xsl:text>
      + <xsl:apply-templates select="tei:death"/>
      <xsl:text> </xsl:text>
      <xsl:apply-templates select="tei:nationality"/>
      <xsl:text> </xsl:text>
      <xsl:apply-templates select="tei:profess"/>
      <xsl:apply-templates select="tei:comment"/>
   </td>
</xsl:template>

<xsl:template match="tei:nationality">
 <a href="pcCountries.html#Country-{@code}">
   <xsl:value-of select="key('CATS',@code)"/>
 </a>
</xsl:template>

<xsl:template match="tei:persName">
 <xsl:text>[</xsl:text><xsl:value-of select="../@sex"/>
 <xsl:text>] </xsl:text>
 <xsl:apply-templates select="tei:foreName"/>
 <xsl:variable name="myName">
   <xsl:apply-templates select="tei:surname"/>
 </xsl:variable>
 &#xA0;<a href="pcNames.html#Name-{$myName}">
  <span class="snm"><xsl:value-of select="$myName"/></span></a>
</xsl:template>

<xsl:template match="tei:decoration">
<xsl:apply-templates select="tei:p"/>
  <xsl:text>
</xsl:text>
  <xsl:for-each select="tei:feature">
   <xsl:call-template name="splitDecl">
     <xsl:with-param name="decl" select="@decls"/>
   </xsl:call-template>
    
  </xsl:for-each>
</xsl:template>

<xsl:template match="tei:div">
 <xsl:variable name="l" select="@lang"/>
<tr valign="top">
<td width="5%"> <strong><xsl:number/></strong></td>
<td width="20%">
   <xsl:call-template name="splitDecl">
     <xsl:with-param name="decl" select="@decls"/>
   </xsl:call-template>
   <xsl:value-of select="key('LANGS',$l)"/>
</td>
<td width="75%">
 <table class="inscrip">
      <xsl:apply-templates/>
 </table>
</td>
</tr>

</xsl:template>

<xsl:template match="tei:ab">
    <tr><td>
  <xsl:choose>
    <xsl:when test="@rend='Alignr'">
      <xsl:attribute name="align">right</xsl:attribute>
    </xsl:when>
    <xsl:when test="@rend='Alignc'">
     <xsl:attribute name="align">center</xsl:attribute>
    </xsl:when>
    <xsl:when test="@rend='Alignl'">
      <xsl:attribute name="align">left</xsl:attribute>
      <xsl:text>&#xA0;&#xA0;</xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:attribute name="align">center</xsl:attribute>
     <xsl:choose>
     <xsl:when test="starts-with(@rend,'indent(')">
    <xsl:attribute name="text-indent">
      <xsl:value-of select="concat(substring-before(substring-after(@rend,'('),')'),'em')"/>
    </xsl:attribute>
  </xsl:when>
  <xsl:when test="starts-with(@rend,'indent')">
    <xsl:attribute name="text-indent">1em</xsl:attribute>
  </xsl:when>
  </xsl:choose>
</xsl:otherwise>
</xsl:choose>
  <xsl:apply-templates/>
</td></tr> 
</xsl:template>

<xsl:template name="splitDecl">
  <xsl:param name="decl"/>
<xsl:choose>
  <xsl:when  test="contains($decl,' ')">
    <xsl:variable name="this" select="substring-before($decl,' ')"/>
    <i><xsl:value-of select="key('CATS',$this)/../@id"/></i>:
    <xsl:value-of select="key('CATS',$this)/tei:catDesc"/>,
    <xsl:call-template name="splitDecl">
     <xsl:with-param name="decl" select="substring-after($decl,' ')"/>
    </xsl:call-template>
  </xsl:when>
  <xsl:otherwise>
    <i><xsl:value-of select="key('CATS',$decl)/../@id"/></i>:
    <xsl:value-of select="key('CATS',$decl)/tei:catDesc"/>.
  </xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template name="sort-by-nationality">


  <xsl:call-template name="outputChunk">
     <xsl:with-param name="ident" select="'pcCountries'"/>
     <xsl:with-param name="content">
    <html>
    <head>
    <title>Protestant Cemetery country index</title>  
    <xsl:call-template name="includeCSS"/>
    </head>
    <body>
<h1>Protestant Cemetery name index</h1>
<xsl:variable name="sorted">
  <xsl:for-each select="tei:TEI/tei:teiHeader/tei:profileDesc/tei:particDesc/tei:person">
    <xsl:sort select="tei:nationality/@code"/>
    <xsl:sort select="substring-before(death/@date,'-')"/>
    <xsl:sort select="tei:persName/surname"/>
     <stoned id="{ancestor::tei:TEI/@id}"
	     snm="{persName/surname}"
	     fnm="{persName/foreName}"
	     yr="{substring-before(death/@date,'-')}"
	     nat="{nationality/@code}"/>
  </xsl:for-each>
</xsl:variable>
<ul>
 <xsl:for-each select="tei:exsl:node-set($sorted)/stoned">
  <xsl:variable name="y" select="@nat"/>
  <xsl:variable name="country">
    <xsl:choose>
    <xsl:when test="$y = ''">none</xsl:when>
    <xsl:otherwise>
         <xsl:value-of select="key('CATS',$y)"/>
    </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:if test="not(preceding-sibling::tei:stoned[$y=@nat])">
<h2><a name="Country-{$y}"/><xsl:value-of select="$country"/> </h2>
<table>
   <xsl:apply-templates select="." mode="summary"/> 
   <xsl:apply-templates select="following-sibling::tei:stoned[$y=@nat]" 
	mode="summary"/>
</table>
  </xsl:if>
 </xsl:for-each>
</ul>
</body></html></xsl:with-param></xsl:call-template></xsl:template>


<xsl:template name="sort-by-name">
  <xsl:call-template name="outputChunk">
     <xsl:with-param name="ident" select="'pcNames'"/>
     <xsl:with-param name="content">
    <html>
    <head>
    <title>Protestant Cemetery name index</title>  
    <xsl:call-template name="includeCSS"/>
    </head>
    <body>
<h1>Protestant Cemetery name index</h1>
<xsl:variable name="sorted">
  <xsl:for-each select="tei:TEI/teiHeader/profileDesc/particDesc/person">
     <xsl:sort select="tei:persName/surname"/>
     <xsl:sort select="substring-before(death/@date,'-')"/>
     <stoned id="{ancestor::tei:TEI/@id}"
	     snm="{persName/surname}"
	     fnm="{persName/foreName}"
	     yr="{substring-before(death/@date,'-')}"
	     nat="{nationality/@code}"/>
  </xsl:for-each>
</xsl:variable>
 <xsl:for-each select="tei:exsl:node-set($sorted)/stoned">
  <xsl:variable name="y" select="@snm"/>
  <xsl:if test="not(preceding-sibling::tei:stoned[$y=@snm])">
<h2><a name="Name-{$y}"/><xsl:value-of select="$y"/> </h2>
<table>
   <xsl:apply-templates select="." mode="summary"/> 
   <xsl:apply-templates select="following-sibling::tei:stoned[$y=@snm]" 
	mode="summary"/>
</table>
  </xsl:if>
 </xsl:for-each>
   </body>
   </html>
   </xsl:with-param>
   </xsl:call-template>
</xsl:template>

<xsl:template name="sort-by-year">
  <xsl:call-template name="outputChunk">
     <xsl:with-param name="ident" select="'pcYears'"/>
     <xsl:with-param name="content">
    <html>
    <head>
    <title>Protestant Cemetery year index</title>  
    <xsl:call-template name="includeCSS"/>
    </head>
    <body>
<h1>Protestant Cemetery year index</h1>
<xsl:variable name="sorted">
  <xsl:for-each select="tei:TEI/teiHeader/profileDesc/particDesc/person">
     <xsl:sort select="substring-before(death/@date,'-')"/>
     <xsl:sort select="tei:persName/surname"/>
     <xsl:sort select="tei:persName/foreName"/>
     <stoned id="{ancestor::tei:TEI/@id}"
	     snm="{persName/surname}"
	     fnm="{persName/foreName}"
	     yr="{substring-before(death/@date,'-')}"
	     nat="{nationality/@code}"/>
  </xsl:for-each>
</xsl:variable>

 <xsl:for-each select="tei:exsl:node-set($sorted)/stoned">
  <xsl:variable name="y" select="@yr"/>
  <xsl:if test="not(preceding-sibling::tei:stoned[$y=@yr])">
<h2><a name="Year-{$y}"/><xsl:value-of select="$y"/></h2>
   <table>
    <xsl:apply-templates select="." mode="summary"/> 
    <xsl:apply-templates select="following-sibling::tei:stoned[$y=@yr]" 
	mode="summary"/>
    </table>
  </xsl:if>
 </xsl:for-each>
   </body>
   </html>
   </xsl:with-param>
   </xsl:call-template>
</xsl:template>

<xsl:template match="tei:stoned" mode="summary">
 <xsl:variable name="y" select="@nat"/>
<tr valign="top">
 <td></td>
 <td><xsl:value-of select="@fnm"/></td>
 <td><xsl:value-of select="@snm"/></td>
 <td><xsl:value-of select="@yr"/></td>
 <td><xsl:value-of select="key('CATS',$y)"/></td>
 <td><a href="{@id}.html">#<xsl:value-of select="@id"/></a> 
 </td>
</tr>
</xsl:template>

<xsl:template match="tei:person" mode="summary">
</xsl:template>

<xsl:template match="tei:person" mode="special">
<xsl:choose>
<xsl:when test="not(preceding-sibling::tei:person)">
     <xsl:apply-templates select=".."/>  
</xsl:when>
<xsl:otherwise>
 <p>See also <xsl:value-of select="tei:persName/foreName"/>
 <xsl:text> </xsl:text><xsl:value-of select="tei:persName/surname"/>
 <xsl:apply-templates select=".." mode="xref"/></p>
</xsl:otherwise>
</xsl:choose>
</xsl:template>


<xsl:template match="tei:death">
 <xsl:variable name="myYear">
   <xsl:value-of select="substring-before(@date,'-')"/>
 </xsl:variable>
 <a href="pcYears.html#Year-{$myYear}">
  <span class="yr"> <xsl:apply-templates   select="@date"/></span></a>
</xsl:template>

<xsl:template match="tei:birth">
  <xsl:apply-templates   select="@date"/>
</xsl:template>

</xsl:stylesheet>


