<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet exclude-result-prefixes="#all" version="2.0"
  xmlns:util="http://www.pnp-software.com/util"
  xmlns:xd="http://www.pnp-software.com/XSLTdoc"
  xmlns:xdt="http://www.pnp-software.com/XSLTdocTemplate"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:import href="stylesheet.xsl"/>
  <xsl:include href="verbatim/xmlverbatimwrapper.xsl"/>
  <xsl:include href="lib/util.xsl"/>

  <xd:doc type="string">
    <xd:short>The directory in which the documentation should be generated.</xd:short>
    <xd:detail> The directory given must be relative to root stylesheet which is
      being processed. This parameter is used to compute the variable
        <code>$targetDirUriAbs</code>. If this parameter is not set the
        <code>$targetDirUriAbs</code> variable defaults a directory 'xsltdoc'
      which is created inside the same directory in which the input stylesheet occurs.<br/>
      <strong>Only used if the input is a stylesheet file. Not used if the input
        is a XSLTdocConfig XML file.</strong>
    </xd:detail>
  </xd:doc>
  <xsl:param name="targetDir" select="false()"/>

  <xd:doc> You can add additional (custom) css stylesheets here, which override
    css rules from the standard css file (XSLTdoc.css) or add new rules. </xd:doc>
  <xsl:param as="xs:string*" name="additionalCSS"/>


  <xd:doc type="stylesheet">
    <xd:short>Core XSLTdoc Stylesheet</xd:short>
    <xd:detail> This stylesheet </xd:detail>
    <xd:author>ibirrer</xd:author>
    <xd:cvsId>$Id: core.xsl,v 1.3 2005/01/04 10:13:05 ibirrer Exp $</xd:cvsId>
    <xd:copyright>2004, P&amp;P Software GmbH</xd:copyright>
  </xd:doc>

  <!-- Default output format, normally overwritten by importing stylesheet -->
  <xsl:output doctype-public="-//W3C//DTD XHTML 1.0 Transitional//EN"
    doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"
    encoding="iso-8859-1" indent="no" method="xml" name="xhtml"
    omit-xml-declaration="yes"/>

  <!-- Output format for logfile -->
  <xsl:output indent="yes" method="xml"/>

  <xd:doc> Root template if XSLTdoConfig file is used </xd:doc>
  <xsl:template match="/xsl:stylesheet">
    <xsl:param as="element()" name="config" tunnel="yes"/>
    <xsl:variable name="targetDirUriAbs"
      select="util:normalizeFolder(resolve-uri(if( $targetDir ) then $targetDir else concat(util:getFolder(base-uri(/)), 'xsltdoc/'), base-uri(/) ))"/>
    <xsl:variable as="xs:anyURI" name="sourceRootUriAbs"
      select="util:getFolder(base-uri(/))"/>
    <xsl:message>Generate documentation in: <xsl:value-of
        select="$targetDirUriAbs"/></xsl:message>
    <xsl:variable as="element()" name="tmpConfig">
      <xsl:element name="{node-name($config)}">
        <xsl:copy-of select="$config/@*"/>
        <xsl:copy-of select="$config/*"/>
        <targetDirUriAbs href="{$targetDirUriAbs}"/>
        <sourceRootUriAbs href="{$sourceRootUriAbs}"/>
      </xsl:element>
    </xsl:variable>

    <xsl:variable as="element()" name="extConfig">
      <xsl:apply-templates mode="enhancePageAttributes" select="$tmpConfig"/>
    </xsl:variable>

    <!-- Read the files to be processed from the config file and save them in a filelist -->
    <xsl:variable as="element()*" name="stylesheetList">
      <pagelist id="stylesheets">
        <xsl:call-template name="buildStylesheetPagelist">
          <xsl:with-param name="stylesheetUri" select="base-uri(/)"/>
          <xsl:with-param name="rootUri" select="$sourceRootUriAbs"/>
          <xsl:with-param name="targetUri" select="$targetDirUriAbs"/>
        </xsl:call-template>
      </pagelist>
    </xsl:variable>

    <xsl:call-template name="generatePages">
      <!-- Append the filelist to the config variable -->
      <xsl:with-param as="element()" name="config"
        select="util:appendElement($extConfig,$stylesheetList)"/>
    </xsl:call-template>
  </xsl:template>

  <xd:doc> Root template if XSLTdoConfig file is used </xd:doc>
  <xsl:template match="/XSLTdocConfig">
    <xsl:param as="element()" name="config" tunnel="yes"/>
    <xsl:variable name="targetDirUriAbs"
      select="resolve-uri(util:normalizeFolder(util:pathToUri(/XSLTdocConfig/TargetDirectory/@path)), base-uri(/) )"/>
    <xsl:variable as="xs:anyURI" name="sourceRootUriAbs">
      <!-- Backwards compatibility to old config files. -->
      <xsl:choose>
        <xsl:when test="/XSLTdocConfig/SourceDirectory">
          <xsl:sequence
            select="resolve-uri(util:normalizeFolder(util:pathToUri(/XSLTdocConfig/SourceDirectory/@path)), base-uri(/))"
          />
        </xsl:when>
        <xsl:otherwise>
          <xsl:sequence
            select="resolve-uri(util:normalizeFolder(util:pathToUri(/XSLTdocConfig/RootStylesheets/@dir)), base-uri(/))"
          />
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:variable as="element()" name="tmpConfig">
      <xsl:element name="{node-name($config)}">
        <xsl:copy-of select="$config/@*"/>
        <xsl:copy-of select="$config/*"/>
        <targetDirUriAbs href="{$targetDirUriAbs}"/>
        <sourceRootUriAbs href="{$sourceRootUriAbs}"/>
        <xsl:if test="AdditionalCSS">
          <additionalCSS>
            <xsl:for-each select="AdditionalCSS/File">
              <file uriAbs="{resolve-uri( @href , $targetDirUriAbs )}">
                <xsl:if test="@media">
                  <xsl:copy-of select="@media"/>
                </xsl:if>
              </file>
            </xsl:for-each>
          </additionalCSS>
        </xsl:if>
      </xsl:element>
    </xsl:variable>

    <xsl:variable as="element()" name="extConfig">
      <xsl:apply-templates mode="enhancePageAttributes" select="$tmpConfig"/>
    </xsl:variable>

    <!-- Read the files to be processed from the config file and save them in a filelist -->
    <xsl:variable as="element()*" name="stylesheetList">
      <xsl:for-each select="RootStylesheets/File">
        <xsl:call-template name="buildStylesheetPagelist">
          <xsl:with-param name="stylesheetUri"
            select="resolve-uri(@href, $sourceRootUriAbs)"/>
          <xsl:with-param name="rootUri" select="$sourceRootUriAbs"/>
          <xsl:with-param name="targetUri" select="$targetDirUriAbs"/>
        </xsl:call-template>
      </xsl:for-each>
    </xsl:variable>

    <!-- Build distinct filelist eliminating file elements with the same @uri attribute -->
    <xsl:variable as="element()" name="distinctStylesheetList">
      <pagelist id="stylesheets">
        <xsl:for-each-group group-by="@srcUriAbs" select="$stylesheetList">
          <xsl:sequence select="."/>
        </xsl:for-each-group>
      </pagelist>
    </xsl:variable>

    <xsl:call-template name="generatePages">
      <!-- Append the filelist to the config variable -->
      <xsl:with-param as="element()" name="config"
        select="util:appendElement(util:appendElement($extConfig,$distinctStylesheetList), /XSLTdocConfig)"
      />
    </xsl:call-template>
  </xsl:template>


  <xsl:template match="*" mode="enhancePageAttributes">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates mode="enhancePageAttributes"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="page" mode="enhancePageAttributes">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:if test="@id">
        <xsl:attribute name="uriAbs">
          <xsl:value-of
            select="resolve-uri( if( @targetFilename ) then @targetFilename else concat(@id, '.html'), ancestor::config/targetDirUriAbs/@href )"
          />
        </xsl:attribute>
        <xsl:attribute name="uriRel">
          <xsl:value-of
            select="if( @targetFilename ) then @targetFilename else concat(@id, '.html')"
          />
        </xsl:attribute>
      </xsl:if>
    </xsl:copy>
  </xsl:template>

  <xd:doc type="void"> Generates each page given in the config parameter. </xd:doc>
  <xsl:template name="generatePages">
    <xsl:param as="element()" name="config"/>
    <!-- To logfile -->
    <xsl:copy-of select="$config"/>
    <xsl:variable as="document-node()" name="htmlTemplate"
      select="doc($config/htmlTemplate/@href)"/>

    <!-- Generate one html page for each page in config -->
    <xsl:for-each select="$config/page">
      <xsl:result-document format="xhtml" href="{@uriAbs}">
        <xsl:apply-templates mode="htmlTemplate" select="$htmlTemplate">
          <xsl:with-param name="config" select="$config" tunnel="yes"/>
          <xsl:with-param name="currentPage" select="." tunnel="yes"/>
        </xsl:apply-templates>
      </xsl:result-document>
    </xsl:for-each>

    <!-- Generate one html page for each stylesheet -->
    <xsl:for-each select="$config/pagelist/page">
      <xsl:result-document format="xhtml" href="{@uriAbs}">
        <xsl:apply-templates mode="htmlTemplate" select="$htmlTemplate">
          <xsl:with-param name="config" select="$config" tunnel="yes"/>
          <xsl:with-param name="currentPage" select="." tunnel="yes"/>
        </xsl:apply-templates>
      </xsl:result-document>
    </xsl:for-each>

    <!-- Generate one highlighted sourcecode page for each stylesheet (using) -->
    <xsl:for-each select="$config/pagelist/page">
      <xsl:result-document format="xhtml" href="{@verbatimUriAbs}">
        <xsl:apply-templates mode="xmlverbwrapper" select="doc(@srcUriAbs)">
          <xsl:with-param name="css-stylesheet">
            <xsl:value-of
              select="concat(util:getRelativeUri( util:getFolder(@uriAbs), $config/targetDirUriAbs/@href ), 'xmlverbatim.css')"
            />
          </xsl:with-param>
        </xsl:apply-templates>
      </xsl:result-document>
    </xsl:for-each>
  </xsl:template>

  <!--
**************************************
 XSLTdoc HTML Templates
**************************************
-->
  <xd:doc>Identity template for XSLTdoc templates</xd:doc>
  <xsl:template match="*" mode="htmlTemplate">
    <xsl:param name="config" tunnel="yes"/>
    <xsl:param name="currentPage" tunnel="yes"/>
    <xsl:element name="{node-name(.)}" namespace="http://www.w3.org/1999/xhtml">
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates mode="htmlTemplate"/>
    </xsl:element>
  </xsl:template>

  <xd:doc> This template replaces the xdt:content element in the html template.</xd:doc>
  <xsl:template match="xdt:content" mode="htmlTemplate"
    xmlns="http://www.w3.org/1999/xhtml">
    <xsl:param as="element()" name="config" tunnel="yes"/>
    <xsl:param as="element()" name="currentPage" tunnel="yes"/>
    <xsl:apply-templates mode="renderContent" select="$currentPage"/>
  </xsl:template>

  <xd:doc> This template replaces the xdt:cssDeclaration element in the html
    template.</xd:doc>
  <xsl:template match="xdt:cssDeclaration" mode="htmlTemplate"
    xmlns="http://www.w3.org/1999/xhtml">
    <xsl:param as="element()" name="config" tunnel="yes"/>
    <xsl:param as="element()" name="currentPage" tunnel="yes"/>
    <link
      href="{concat(util:getRelativeUri( util:getFolder($currentPage/@uriAbs), $config/targetDirUriAbs/@href ), 'XSLTdoc.css')}"
      rel="stylesheet" type="text/css"/>
    <xsl:if test="$config/additionalCSS">
      <xsl:for-each select="$config/additionalCSS/file">
        <link
          href="{util:getRelativeUriFiles( @uriAbs, $currentPage/@uriAbs, true() )}"
          rel="stylesheet" type="text/css">
          <xsl:if test="@media">
            <xsl:copy-of select="@media"/>
          </xsl:if>
        </link>
      </xsl:for-each>
    </xsl:if>
  </xsl:template>

  <xd:doc> Renders the site title</xd:doc>
  <xsl:template match="xdt:siteTitle" mode="htmlTemplate"
    xmlns="http://www.w3.org/1999/xhtml">
    <xsl:param as="element()" name="config" tunnel="yes"/>
    <xsl:param as="element()" name="currentPage" tunnel="yes"/>
    <xsl:choose>
      <xsl:when test="$config/XSLTdocConfig">
        <xsl:value-of
          select="concat($config/XSLTdocConfig/Title,' - ', $currentPage/@label)"
        />
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="concat('XSLTdoc - ', $currentPage/@label)"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xd:doc> This template replaces the xdt:menuLinks element in the html
    template.</xd:doc>
  <xsl:template match="xdt:menuLinks" mode="htmlTemplate"
    xmlns="http://www.w3.org/1999/xhtml">
    <xsl:param as="element()" name="config" tunnel="yes"/>
    <xsl:param as="element()" name="currentPage" tunnel="yes"/>
    <xsl:for-each select="$config/page[@menu='yes']">
      <xsl:choose>
        <xsl:when test="$currentPage is .">
          <a class="menuLinkCurrent"
            href="{util:getRelativeUriFiles( @uriAbs, $currentPage/@uriAbs, true() )}">
            <xsl:value-of select="@label"/>
          </a>
        </xsl:when>
        <xsl:otherwise>
          <a class="menuLink"
            href="{util:getRelativeUriFiles( @uriAbs, $currentPage/@uriAbs, true() )}">
            <xsl:value-of select="@label"/>
          </a>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <xd:doc> This stylesheet renders the content for pages definded in the
    'stylesheet' pagelist. It just applies the template rules to the root
    element of the stylesheets linked in the page element. This calls the
    template rules defined in stylesheet.xsl.</xd:doc>
  <xsl:template match="page[parent::pagelist[@id='stylesheets']]"
    mode="renderContent">
    <xsl:param as="element()" name="config" tunnel="yes"/>
    <xsl:param as="element()" name="currentPage" tunnel="yes"/>
    <!-- Calls the templates defined in stylesheet.xsl -->
    <xsl:apply-templates mode="stylesheet" select="doc(@srcUriAbs)"/>
  </xsl:template>

  <xd:doc> This templates creates the content for a page with the name
    'stylesheetList'.</xd:doc>
  <xsl:template match="page[@id='stylesheetList']" mode="renderContent"
    xmlns="http://www.w3.org/1999/xhtml">
    <xsl:param as="element()" name="config" tunnel="yes"/>
    <xsl:param as="element()" name="currentPage" tunnel="yes"/>
    <div id="stylesheetList">
      <h1>Stylesheet List</h1>
      <xsl:for-each select="$config/pagelist[@id='stylesheets']/page">
        <xsl:sort select="@srcDirAbs"/>
        <xsl:sort select="@srcFilename"/>
        <div class="listItem">
          <span class="declName">
            <a class="declLink" href="{@uriRel}">
              <xsl:value-of select="@srcUriRel"/>
            </a>
          </span>
          <div class="shortDoc">
            <xsl:apply-templates mode="printShortDescription"
              select="doc(@srcUriAbs)/xsl:stylesheet"/>
          </div>
        </div>
      </xsl:for-each>
    </div>
  </xsl:template>

  <xd:doc> This templates creates the content for a page with the name
    'functionTemplateList'.</xd:doc>
  <xsl:template match="page[@id='functionTemplateList']" mode="renderContent"
    xmlns="http://www.w3.org/1999/xhtml">
    <xsl:param as="element()" name="config" tunnel="yes"/>
    <xsl:param as="element()" name="currentPage" tunnel="yes"/>
    <div id="functionTemplateList">
      <h1>Functions/Templates List</h1>

      <xsl:variable as="element()*" name="allFunctions">
        <xsl:for-each select="$config/pagelist[@id='stylesheets']/page">
          <xsl:variable name="page" select="."/>
          <xsl:for-each select="doc(@srcUriAbs)/xsl:stylesheet/xsl:function">
            <xsl:sequence select="."/>
            <xsl:sequence select="$page"/>
          </xsl:for-each>
        </xsl:for-each>
      </xsl:variable>

      <xsl:variable as="element()*" name="allTemplates">
        <xsl:for-each select="$config/pagelist[@id='stylesheets']/page">
          <xsl:variable name="page" select="."/>
          <xsl:for-each
            select="doc(@srcUriAbs)/xsl:stylesheet/xsl:template[@name]">
            <xsl:sequence select="."/>
            <xsl:sequence select="$page"/>
          </xsl:for-each>
        </xsl:for-each>
      </xsl:variable>

      <div id="namedTemplatesSummary">
        <h2>Templates</h2>
        <xsl:for-each select="$allTemplates[self::xsl:template]">
          <xsl:sort select="@name"/>
          <xsl:variable name="pos" select="position()"/>
          <div class="listItem">
            <xsl:apply-templates mode="printDeclaration" select=".">
              <xsl:with-param name="link"
                select="concat($allTemplates[self::page][$pos]/@uriRel, '#', generate-id(.))"/>
              <xsl:with-param name="verbatimUriRel"
                select="$allTemplates[self::page][$pos]/@verbatimUriRel"/>
            </xsl:apply-templates>
            <div class="shortDoc">
              <xsl:apply-templates mode="printShortDescription" select="."/>
            </div>
          </div>
        </xsl:for-each>
      </div>

      <div id="functionsSummary">
        <h2>Functions</h2>
        <xsl:for-each select="$allFunctions[self::xsl:function]">
          <xsl:variable name="pos" select="position()"/>
          <div class="listItem">
            <xsl:apply-templates mode="printDeclaration" select=".">
              <xsl:with-param name="link"
                select="concat($allFunctions[self::page][$pos]/@uriRel, '#', generate-id(.))"/>
              <xsl:with-param name="verbatimUriRel"
                select="$allFunctions[self::page][$pos]/@verbatimUriRel"/>
            </xsl:apply-templates>
            <div class="shortDoc">
              <xsl:apply-templates mode="printShortDescription" select="."/>
            </div>
          </div>
        </xsl:for-each>
      </div>
    </div>
  </xsl:template>

  <xd:doc> This templates creates the content for a page with the name
    'mainPage'.</xd:doc>
  <xsl:template match="page[@id='mainPage']" mode="renderContent"
    xmlns="http://www.w3.org/1999/xhtml">
    <xsl:param as="element()" name="config" tunnel="yes"/>
    <xsl:param as="element()" name="currentPage" tunnel="yes"/>
    <div id="mainPage">
      <h1>
        <xsl:value-of select="$config/XSLTdocConfig/Title"/>
      </h1>
      <xsl:apply-templates mode="namspace2xhtml"
        select="$config/XSLTdocConfig/Introduction/node()"/>
    </div>
  </xsl:template>

  <xsl:template match="*" mode="namspace2xhtml">
    <xsl:element name="{node-name(.)}" namespace="http://www.w3.org/1999/xhtml">
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates mode="XdocTags"/>
    </xsl:element>
  </xsl:template>


  <!-- 
****************************************
* Named Templates
****************************************
 -->
  <xd:doc> Builds a list of all stylesheet files by following the include and
    import links in each stylesheet. <xd:param name="stylesheetUri">The URI of
      the stylesheet from which indcludes/imports should be followed to find
      files.</xd:param>
    <xd:param name="rootUri">The root directory to which relative paths should
      be built to.</xd:param>
  </xd:doc>
  <xsl:template as="element()*" name="buildStylesheetPagelist">
    <xsl:param as="xs:anyURI" name="stylesheetUri" select="base-uri(.)"/>
    <xsl:param as="xs:anyURI" name="rootUri"/>
    <xsl:param as="xs:anyURI" name="targetUri"/>
    <xsl:variable name="srcUriAbs" select="util:normalizeUri($stylesheetUri)"/>
    <xsl:variable name="srcDirAbs" select="util:getFolder($srcUriAbs)"/>
    <xsl:variable name="srcFilename" select="util:getFile($srcUriAbs)"/>
    <xsl:variable name="srcDirRel"
      select="util:getRelativeUri($rootUri, util:getFolder($srcUriAbs))"/>
    <xsl:variable name="srcUriRel" select="concat($srcDirRel, $srcFilename)"/>
    <xsl:variable name="uriAbs"
      select="concat($targetUri, $srcDirRel, $srcFilename, '.xd.html')"/>
    <xsl:variable name="uriRel"
      select="concat($srcDirRel, $srcFilename, '.xd.html')"/>
    <xsl:variable name="verbatimUriAbs"
      select="concat($targetUri, $srcDirRel, $srcFilename, '.src.html')"/>
    <xsl:variable name="verbatimUriRel"
      select="concat($srcDirRel, $srcFilename, '.src.html')"/>
    <xsl:message> srcUriAbs: <xsl:value-of select="$srcUriAbs"/>; srcDirAbs:
        <xsl:value-of select="$srcDirAbs"/>; srcFilename: <xsl:value-of
        select="$srcFilename"/>; srcDirRel: <xsl:value-of select="$srcDirRel"/>;
      srcUriRel: <xsl:value-of select="$srcUriRel"/>; uriAbs: <xsl:value-of
        select="$uriAbs"/>; uriRel: <xsl:value-of select="$uriRel"/>;
      verbatimUriAbs: <xsl:value-of select="$verbatimUriAbs"/>; verbatimUriRel:
        <xsl:value-of select="$verbatimUriRel"/>;</xsl:message>
    <page label="{$srcFilename}" srcDirAbs="{$srcDirAbs}"
      srcFilename="{$srcFilename}" srcUriAbs="{$srcUriAbs}"
      srcUriRel="{$srcUriRel}" uriAbs="{$uriAbs}" uriRel="{$uriRel}"
      verbatimUriAbs="{$verbatimUriAbs}" verbatimUriRel="{$verbatimUriRel}"> </page>

    <!-- Follow import/includes -->
    <xsl:for-each
      select="document($srcUriAbs)/xsl:stylesheet/xsl:include |
		document($srcUriAbs)/xsl:stylesheet/xsl:import">
      <xsl:call-template name="buildStylesheetPagelist">
        <xsl:with-param name="stylesheetUri"
          select="resolve-uri(@href, base-uri(.))"/>
        <xsl:with-param name="rootUri" select="$rootUri"/>
        <xsl:with-param name="targetUri" select="$targetUri"/>
      </xsl:call-template>
    </xsl:for-each>
  </xsl:template>

</xsl:stylesheet>
