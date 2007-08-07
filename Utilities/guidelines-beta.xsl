<xsl:stylesheet
  exclude-result-prefixes="xlink dbk rng tei teix xhtml a edate estr html pantor xd xs xsl"
  extension-element-prefixes="exsl estr edate" 
  version="1.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xlink="http://www.w3.org/1999/xlink"
  xmlns:dbk="http://docbook.org/ns/docbook"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:xhtml="http://www.w3.org/1999/xhtml"
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
  xmlns:edate="http://exslt.org/dates-and-times"
  xmlns:estr="http://exslt.org/strings" xmlns:exsl="http://exslt.org/common"
  xmlns:html="http://www.w3.org/1999/xhtml"
  xmlns:pantor="http://www.pantor.com/ns/local"
  xmlns:xd="http://www.pnp-software.com/XSLTdoc"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  
<xsl:import href="odd2htmlp5.xsl"/>
<xsl:param name="lang"/>
<xsl:param name="doclang"/>
<xsl:param name="footnoteFile">false</xsl:param>
<xsl:param name="cssFile">guidelines-beta.css</xsl:param>
<xsl:param name="displayMode">both</xsl:param>

  <xsl:template name="includeCSS">
    <link href="{$cssFile}" rel="stylesheet" type="text/css"/>
    <xsl:if test="not($cssPrintFile='')">
      <link rel="stylesheet" media="print" type="text/css" href="{$cssPrintFile}"/>
    </xsl:if>
  </xsl:template>

<xsl:template name="generateSubTitle">
  <xsl:value-of select="tei:head"/>
</xsl:template>

  
 <xsl:template name="pageTableHeader">
    <xsl:param name="mode"/>
	<table width="100%" border="0">
	  <tr>
	    <td height="100" class="bgimage" onClick="window.location='{$homeURL}'" cellpadding="0">
	      <h1 class="maintitle"><xsl:call-template name="generateTitle"/></h1>
	      <h2 class="subtitle"><xsl:call-template name="generateSubTitle"/></h2>
	    </td>
	  </tr>
	</table>
 </xsl:template>

<xsl:template name="printLink"/>


<xsl:template match="tei:titlePage" mode="paging">
  <xsl:apply-templates select="."/>
</xsl:template>

<xsl:template match="/div">
</xsl:template>  




  <xsl:template name="bitOut">
    <xsl:param name="grammar"/>
    <xsl:param name="content"/>
    <xsl:param name="element">pre</xsl:param>
    <xsl:choose>
      <xsl:when test="$displayMode='both'">
        <div class="displayRelax">
          <button class="displayRelax"
            onclick="togglerelax(this)">Display
              RNG</button>
          <pre class="eg_rng" style="display:none">
            <xsl:apply-templates mode="verbatim"
              select="exsl:node-set($content)/*/*"/>
          </pre>
          <pre class="eg_rnc" style="display:block">
            <xsl:call-template name="make-body-from-r-t-f">
              <xsl:with-param name="schema">
                <xsl:for-each select="exsl:node-set($content)/*">
                  <xsl:call-template name="make-compact-schema"/>
                </xsl:for-each>
              </xsl:with-param>
            </xsl:call-template>
          </pre>
        </div>
      </xsl:when>
      <xsl:when test="$displayMode='rng'">
        <xsl:element name="{$element}">
          <xsl:attribute name="class">eg</xsl:attribute>
          <xsl:apply-templates mode="verbatim"
            select="exsl:node-set($content)/*/*"/>
        </xsl:element>
      </xsl:when>
      <xsl:when test="$displayMode='rnc'">
        <xsl:element name="{$element}">
          <xsl:attribute name="class">eg</xsl:attribute>
          <xsl:call-template name="make-body-from-r-t-f">
            <xsl:with-param name="schema">
              <xsl:for-each select="exsl:node-set($content)/*">
                <xsl:call-template name="make-compact-schema"/>
              </xsl:for-each>
            </xsl:with-param>
          </xsl:call-template>
        </xsl:element>
      </xsl:when>
      <xsl:otherwise>
        <xsl:element name="{$element}">
          <xsl:attribute name="class">eg</xsl:attribute>
          <xsl:for-each select="exsl:node-set($content)/*">
            <xsl:apply-templates mode="literal"/>
          </xsl:for-each>
        </xsl:element>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  
  <xsl:template name="javascriptHook">
    <script type="text/javascript">
      <xsl:text disable-output-escaping="yes">
        function togglerelax (el) {
        if (el.innerHTML == 'Display RNC') {
        el.innerHTML = 'Display RNG';
        }
        else
        {
        el.innerHTML = 'Display RNC';
        }
        var div = el.parentNode; 
        for (j=0;j&lt;div.childNodes.length;j++)
        {
        if (div.childNodes[j].nodeType != 1) continue;
        if (div.childNodes[j].nodeName != 'PRE') continue;
        var thisone=div.childNodes[j];
        var state=thisone.style.display;
        if (state == 'block')
        {  
        thisone.style.display='none'; 
        }
        else
        {  
        thisone.style.display='block';
        }
        }
        }
        
      </xsl:text>
    </script>
  </xsl:template>



<!-- JC Edit: Add permalinks to H2 need to overwrite the entire
blasted template for main content  -->
  
  
  
  <xsl:template name="mainFrame">
    <xsl:param name="currentID"/>
    <xsl:choose>
      <xsl:when test="$currentID='current'">
        <xsl:apply-templates/>
      </xsl:when>
      <xsl:when test="$currentID='' and $splitLevel=-1">
        <xsl:apply-templates/>
      </xsl:when>
      <xsl:when test="$currentID='' and $virtualPages='true'">
        <xsl:apply-templates/>
      </xsl:when>
      <xsl:when test="self::teiCorpus.2">
        <xsl:call-template name="corpusBody"/>
      </xsl:when>
      <xsl:when test="$currentID=''">
        <!-- we need to locate the first interesting object in the file, ie
          the first grandchild of <text > -->
        <xsl:for-each
          select=" descendant-or-self::tei:TEI/tei:text/tei:*[1]/*[1]">
          <xsl:apply-templates mode="paging" select="."/>
          <xsl:if test="$autoToc='true'">
            <xsl:if test="following-sibling::tei:div/tei:head">
              <xsl:call-template name="contentsHeading"/>
              <ul class="toc">
                <xsl:apply-templates mode="maketoc"
                  select="following-sibling::tei:div">
                  <xsl:with-param name="forcedepth" select="'0'"/>
                </xsl:apply-templates>
              </ul>
            </xsl:if>
          </xsl:if>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <xsl:choose>
          <xsl:when test="count(key('IDS',$currentID))&gt;0">
            <xsl:for-each select="key('IDS',$currentID)">
  
              <!-- JC edit making permalinks add @id to html:h2 and
                then html:a to point to it.  
                 -->
              
              <h2 id="{$currentID}">
                <xsl:apply-templates mode="xref" select="."/>  
                <a href="#{$currentID}" class="permalink"> &#x00B6;</a>
              </h2>
              <xsl:call-template name="doDivBody"/>
              <xsl:if test="$bottomNavigationPanel='true'">
                <xsl:call-template name="xrefpanel">
                  <xsl:with-param name="homepage"
                    select="concat($masterFile,$standardSuffix)"/>
                  <xsl:with-param name="mode" select="local-name(.)"/>
                </xsl:call-template>
              </xsl:if>
            </xsl:for-each>
          </xsl:when>
          <xsl:otherwise>
            <!-- the passed ID is a pseudo-XPath expression
              which starts below TEI/tei:text.
              The real XPath syntax is changed to avoid problems
            -->
            <xsl:choose>
              <xsl:when test="ancestor-or-self::tei:TEI/tei:group/tei:text">
                <xsl:apply-templates mode="xpath"
                  select="ancestor-or-self::tei:TEI/tei:group/tei:text">
                  <xsl:with-param name="xpath" select="$currentID"/>
                </xsl:apply-templates>
              </xsl:when>
              <xsl:otherwise>
                <xsl:apply-templates mode="xpath"
                  select="ancestor-or-self::tei:TEI/tei:text">
                  <xsl:with-param name="xpath" select="$currentID"/>
                </xsl:apply-templates>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
    
    <xsl:call-template name="partialFootNotes">
      <xsl:with-param name="currentID" select="$currentID"/>
    </xsl:call-template>
    
    <xsl:call-template name="stdfooter"/>
  </xsl:template>
  
  
  
  

  
       

</xsl:stylesheet>


