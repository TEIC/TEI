<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet exclude-result-prefixes="#all" version="2.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:util="http://www.pnp-software.com/util"
  xmlns:xd="http://www.pnp-software.com/XSLTdoc"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xd:doc type="stylesheet">
    <xd:short>Creates a HTML page for a stylesheet.</xd:short>
    <xd:detail> This stylesheet creates the xhtml output for one stylesheet. It
      calls sub-templates to build documentation parts for different parts of a
      stylesheet (templates, functions etc.). Note that the default namespace of
      this stylesheet is set to http://www.w3.org/1999/xhtml. That means that
      any literal element is of this namespace if not specified specificely! </xd:detail>
    <xd:author>ibirrer</xd:author>
    <xd:cvsId>$Id$</xd:cvsId>
    <xd:copyright>2004, P&amp;P Software GmbH</xd:copyright>
  </xd:doc>

  <xd:doc>
    <xd:short>Calls sub-templates for each part of the stylesheet
    documentation.</xd:short>
  </xd:doc>

  <xsl:template match="/xsl:stylesheet" mode="stylesheet">
    <xsl:apply-templates mode="stylesheetDetail" select="."/>
    <xsl:apply-templates mode="parametersSummary" select="."/>
    <xsl:apply-templates mode="matchTemplatesSummary" select="."/>
    <xsl:apply-templates mode="namedTemplatesSummary" select="."/>
    <xsl:apply-templates mode="functionsSummary" select="."/>
    <xsl:apply-templates mode="parametersDetail" select="."/>
    <xsl:apply-templates mode="matchTemplatesDetail" select="."/>
    <xsl:apply-templates mode="namedTemplatesDetail" select="."/>
    <xsl:apply-templates mode="functionsDetail" select="."/>
  </xsl:template>

  <xd:doc> Extracts the short description from a xd:doc element. Everything
    before the first period is considered as short description. If the string
    doesn't contain a period, the whole string is returned. <xd:param name="doc"
      type="string">xd:doc element</xd:param>
  </xd:doc>
  <xsl:template name="extractShortDescription">
    <xsl:param name="doc"/>
    <xsl:variable name="shortDesc"
      select="substring-before(string($doc/text()),'.')"/>
    <xsl:choose>
      <xsl:when test="string-length($shortDesc) &lt;= 0">
        <xsl:value-of select="$doc/text()"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$shortDesc"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xd:doc> Extracts the detail description from string. Everything after the
    first period is considered as detail description. If no detail description
    can be extracted, the empty string is returned. <xd:param name="doc"
      type="string"> xd:doc element </xd:param>
  </xd:doc>
  <xsl:template name="extractDetailDescription">
    <xsl:param as="element()" name="doc"/>
    <xsl:variable name="detailDesc"
      select="substring-after(string($doc/text()),'.')"/>
    <xsl:choose>
      <xsl:when test="string-length($detailDesc) &lt;= 0">
        <xsl:text>&#160;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$detailDesc"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xd:doc>
    <xd:short>Prints the detail description of a xd:doc element.</xd:short>
    <xd:detail> If no detail description is found, the string &quot;No
      detail description available&quot; is printed </xd:detail>
  </xd:doc>
  <xsl:template match="xsl:function | xsl:template | xsl:stylesheet | xsl:param"
    mode="printDetailDescription">
    <xsl:variable as="element(xd:doc)?" name="doc" select="xd:getDoc(.)"/>

    <xsl:choose>
      <xsl:when test="count($doc) != 0">
        <div class="detailDescr">
          <!-- xd documentation exists, find detail description -->
          <xsl:choose>
            <xsl:when test="$doc/xd:detail">
              <xsl:apply-templates mode="XdocTags" select="$doc/xd:detail"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:call-template name="extractDetailDescription">
                <xsl:with-param name="doc" select="$doc"/>
              </xsl:call-template>
            </xsl:otherwise>
          </xsl:choose>
        </div>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text/>
      </xsl:otherwise>
    </xsl:choose>

  </xsl:template>

  <!-- 
  ***************************************
  * Templates with mode 'printProperties'
  ***************************************
  -->
  <xd:doc>
    <xd:short>Prints the properties of a xd:doc element.</xd:short>
  </xd:doc>
  <xsl:template match="xsl:function | xsl:template | xsl:stylesheet"
    mode="printProperties">
    <xsl:variable as="element(xd:doc)?" name="doc" select="xd:getDoc(.)"/>
    <xsl:variable name="htmlResult">
      <!-- Simple properties -->
      <xsl:apply-templates mode="printProperty" select="$doc/*"/>

      <!-- Parameters -->
      <xsl:if test="xsl:param and not(self::xsl:stylesheet)">
        <div class="property">
          <div class="propertyCaption">Parameters:</div>
          <div class="propertyContent">
            <xsl:for-each select="xsl:param">
              <!-- List parameters -->
              <div class="parameterDetail">
                <xsl:copy-of select="xd:printParamDeclaration(., $doc)"/>
                <span class="paramDescr"> - <xsl:apply-templates mode="XdocTags"
                    select="$doc/xd:param[@name=current()/@name]"/></span>
              </div>
            </xsl:for-each>
          </div>
        </div>
      </xsl:if>

    </xsl:variable>
    <xsl:if test="count($htmlResult/*) != 0">
      <div class="properties">
        <xsl:copy-of select="$htmlResult"/>
      </div>
    </xsl:if>
  </xsl:template>

  <!-- 
  ***************************************
  * Templates with mode 'printProperty'
  ***************************************
  -->
  <xd:doc>
    <xd:short>Overwrites XSLT default rules.</xd:short>
    <xd:detail>This ensures that tags not handled in the mode
      <i>printProperty</i> are not printed.</xd:detail>
  </xd:doc>
  <xsl:template match="*" mode="printProperty"/>

  <!-- **** -->
  <xd:doc>
    <xd:short>Prints the short description of a documented xsl function or
      template.</xd:short>
    <xd:detail> If there's nn xd:doc element dedined or he xd:doc element does
      not contain a short description, the string &quot;No short description
      available&quot; is printed. </xd:detail>
  </xd:doc>
  <xsl:template match="*" mode="printShortDescription">
    <xsl:variable as="element(xd:doc)?" name="doc" select="xd:getDoc(.)"/>
    <div class="shortDescr">
      <xsl:choose>
        <xsl:when test="count($doc) != 0">
          <!-- xd documentation exists, find short description -->
          <xsl:choose>
            <xsl:when test="$doc/xd:short">
              <xsl:apply-templates mode="XdocTags" select="$doc/xd:short"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:call-template name="extractShortDescription">
                <xsl:with-param name="doc" select="$doc"/>
              </xsl:call-template>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:when>
        <xsl:otherwise>No short description available</xsl:otherwise>
      </xsl:choose>
    </div>
  </xsl:template>

  <xd:doc>
    <xd:short>Returns the xd:doc node of an element.</xd:short>
    <xd:detail/>
    <xd:param name="element"> The element can be one of the following: <ul>
        <li>xsl:stylesheet</li>
        <li>xsl:template</li>
        <li>xsl:function</li>
      </ul> Returns the empty sequence if no xd:doc element was found for the
      given element. </xd:param>
  </xd:doc>
  <xsl:function as="element(xd:doc)?" name="xd:getDoc">
    <xsl:param as="element(*)" name="element"/>
    <xsl:choose>
      <xsl:when test="$element[self::xsl:stylesheet]">
        <xsl:sequence select="$element/xd:doc[@type='stylesheet']"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence
          select="$element/preceding-sibling::*[1][self::xd:doc and (@type != 'stylesheet' or not(@type)) ]"
        />
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>

  <xd:doc> Prints the declaration of a function or template. <xd:param
      name="link">If this parameter equals to true() it adds the declaration as
      a link to the detailied declaration</xd:param>
  </xd:doc>
  <xsl:template match="xsl:function | xsl:template | xsl:param"
    mode="printDeclaration">
    <xsl:param name="link" select="false()"/>
    <xsl:param name="verbatimUriRel" select="false()"/>
    <xsl:variable as="element(xd:doc)?" name="doc" select="xd:getDoc(.)"/>
    <xsl:variable name="verbatimLink"
      select="if( $verbatimUriRel ) then concat( $verbatimUriRel,'#',  generate-id(if ($doc) then $doc else .)) else concat(util:getFile(base-uri(.)),'.src.html#',generate-id(if ($doc) then $doc else .))"/>

    <xsl:variable name="name" select="if ( @match ) then @match else @name"/>
    <div class="declaration">
      <!-- Declaration type -->
      <xsl:choose>
        <xsl:when test="$doc/@type">
          <span class="paramType"><xsl:value-of select="$doc/@type"
          />&#160;</span>
        </xsl:when>
        <xsl:when test="@as and not($doc/@type)">
          <span class="paramType"><xsl:value-of select="@as"/>&#160;</span>
        </xsl:when>
      </xsl:choose>
      <!-- Declaration Name -->
      <span class="declName">
        <xsl:choose>
          <xsl:when test="$link">
            <a class="declLink" href="{$link}">
              <xsl:value-of select="$name"/>
            </a>
          </xsl:when>
          <xsl:otherwise>
            <a name="{generate-id(.)}">
              <xsl:value-of select="$name"/>
            </a>
          </xsl:otherwise>
        </xsl:choose>
      </span>
      <!-- Mode and Params -->
      <xsl:if test="xsl:param or @mode">
        <xsl:text> (</xsl:text>
        <xsl:if test="xsl:param">
          <span class="declCaption">param: </span>
          <xsl:for-each select="xsl:param">
            <xsl:copy-of select="xd:printParamDeclaration(., $doc)"/>
            <xsl:if test="position() != last()">,&#160;</xsl:if>
          </xsl:for-each>
        </xsl:if>
        <!-- Mode -->
        <xsl:if test="@mode">
          <span class="declCaption">mode: </span>
          <span class="paramValue">
            <xsl:value-of select="@mode"/>
          </span>
        </xsl:if>
        <xsl:text>)</xsl:text>
      </xsl:if>
      <!-- Link to source -->
      <xsl:text> - </xsl:text>
      <a class="sourceLink" href="{$verbatimLink}">source</a>
    </div>
  </xsl:template>

  <xd:doc> </xd:doc>
  <xsl:function name="xd:printParamDeclaration">
    <xsl:param name="param"/>
    <xsl:param name="doc"/>
    <!-- Param type -->
    <xsl:choose>
      <xsl:when test="$doc/xd:param[@name=$param/@name]/@type">
        <span class="paramType"><xsl:value-of
            select="$doc/xd:param[@name=$param/@name]/@type"/>&#160;</span>
      </xsl:when>
      <xsl:when test="$param/@as">
        <span class="paramType"><xsl:value-of select="$param/@as"
        />&#160;</span>
      </xsl:when>
    </xsl:choose>
    <!-- Param name -->
    <span class="paramName">
      <xsl:value-of select="$param/@name"/>
    </span>
  </xsl:function>

  <xd:doc> Prints the short form of the decalaration of a template. This
    includes the parameters and the mode. <xd:param name="doc" type="node-set"
      >The xd:doc node-set</xd:param>
    <xd:param name="template" type="node-set"> The xsl:template node-set for
      which the <b>declaration</b> should be printed </xd:param>
  </xd:doc>
  <xsl:template name="printTemplateDeclaration">
    <xsl:param name="doc"/>
    <xsl:param name="template"/>
    <xsl:if test="$template/xsl:param or $template/@mode">
      <xsl:text> (</xsl:text>
      <xsl:if test="$template/xsl:param">
        <span class="form">param: </span>
        <xsl:for-each select="$template/xsl:param">
          <xsl:if test="$doc/xd:param[@name=current()/@name]">
            <span class="parameterType">
              <xsl:value-of select="$doc/xd:param[@name=current()/@name]/@type"
              />&#160;</span>
          </xsl:if>
          <span class="parameterName">
            <xsl:value-of select="@name"/>
            <xsl:if test="position() != last()">,&#160;</xsl:if>
          </span>
        </xsl:for-each>
      </xsl:if>
      <xsl:if test="@mode">
        <span class="form">mode: </span>
        <span class="parameterValue">
          <xsl:value-of select="$template/@mode"/>
        </span>
      </xsl:if>
      <xsl:text>)</xsl:text>
    </xsl:if>
  </xsl:template>

  <xd:doc>Outputs title for function details and creates detailed documentation
    for each function</xd:doc>
  <xsl:template match="xsl:stylesheet" mode="functionsDetail">
    <xsl:if test="xsl:function">
      <div id="functionsDetail">
        <h2>Functions Detail</h2>
        <xsl:for-each select="xsl:function">
          <xsl:sort select="@name"/>
          <div class="listItem">
            <xsl:apply-templates mode="printDeclaration" select="."/>
            <div class="detailDoc">
              <xsl:apply-templates mode="printShortDescription" select="."/>
              <xsl:apply-templates mode="printDetailDescription" select="."/>
              <xsl:apply-templates mode="printProperties" select="."/>
            </div>
          </div>
        </xsl:for-each>
      </div>
    </xsl:if>
  </xsl:template>

  <xd:doc>Outputs title for named template details and creates detailed
    documentation for each named template</xd:doc>
  <xsl:template match="xsl:stylesheet" mode="namedTemplatesDetail">
    <xsl:if test="xsl:template[@name]">
      <div id="namedTemplatesDetail">
        <h2>Named Templates Detail</h2>
        <xsl:for-each select="xsl:template[@name]">
          <xsl:sort select="@name"/>
          <div class="listItem">
            <xsl:apply-templates mode="printDeclaration" select="."/>
            <div class="detailDoc">
              <xsl:apply-templates mode="printShortDescription" select="."/>
              <xsl:apply-templates mode="printDetailDescription" select="."/>
              <xsl:apply-templates mode="printProperties" select="."/>
            </div>
          </div>
        </xsl:for-each>
      </div>
    </xsl:if>
  </xsl:template>

  <xd:doc>Outputs title for match template details and creates detailed
    documentation for each match template</xd:doc>
  <xsl:template match="xsl:stylesheet" mode="matchTemplatesDetail">
    <xsl:if test="xsl:template[@match]">
      <div id="matchTemplatesDetail">
        <h2>Match Templates Detail</h2>
        <xsl:for-each select="xsl:template[@match]">
          <xsl:sort select="concat(@mode,@match)"/>
          <div class="listItem">
            <xsl:apply-templates mode="printDeclaration" select="."/>
            <div class="detailDoc">
              <xsl:apply-templates mode="printShortDescription" select="."/>
              <xsl:apply-templates mode="printDetailDescription" select="."/>
              <xsl:apply-templates mode="printProperties" select="."/>
            </div>
          </div>
        </xsl:for-each>
      </div>
    </xsl:if>
  </xsl:template>

  <xd:doc>Outputs title for match template details and creates detailed
    documentation for each match template</xd:doc>
  <xsl:template match="xsl:stylesheet" mode="parametersDetail">
    <xsl:if test="xsl:param">
      <div id="parametersDetail">
        <h2>Parameters Detail</h2>
        <xsl:for-each select="xsl:param">
          <xsl:sort select="@name"/>
          <div class="listItem">
            <xsl:apply-templates mode="printDeclaration" select="."/>
            <div class="detailDoc">
              <xsl:apply-templates mode="printShortDescription" select="."/>
              <xsl:apply-templates mode="printDetailDescription" select="."/>
              <xsl:apply-templates mode="printProperties" select="."/>
            </div>
          </div>
        </xsl:for-each>
      </div>
    </xsl:if>
  </xsl:template>

  <xd:doc>Prints details of the stylesheet.</xd:doc>
  <xsl:template match="xsl:stylesheet" mode="stylesheetDetail"
    xmlns="http://www.w3.org/1999/xhtml">
    <div id="stylesheetDetail" xmlns="http://www.w3.org/1999/xhtml">
      <!-- ************* Title ********************** -->
      <h1>
        <xsl:value-of select="util:getFile(base-uri(.))"/>
      </h1>
      <div class="detailDoc">
        <!-- ************* Includes ******************* -->
        <xsl:if test="xsl:include">
          <div id="includes">
            <h2>Includes</h2>
            <ul>
              <xsl:for-each select="xsl:include">
                <li>
                  <a class="filelink" href="{@href}.xd.html">
                    <xsl:value-of select="@href"/>
                  </a>
                </li>
              </xsl:for-each>
            </ul>
          </div>
        </xsl:if>
        <!-- ************* Imorts ********************* -->
        <xsl:if test="xsl:import">
          <div id="imports">
            <h2>Imports</h2>
            <ul>
              <xsl:for-each select="xsl:import">
                <li>
                  <a class="filelink" href="{@href}.xd.html">
                    <xsl:value-of select="@href"/>
                  </a>
                </li>
              </xsl:for-each>
            </ul>
          </div>
        </xsl:if>

        <xsl:apply-templates mode="printShortDescription" select="."/>
        <xsl:apply-templates mode="printDetailDescription" select="."/>
        <xsl:apply-templates mode="printProperties" select="."/>
      </div>
      <!-- detailDoc -->
    </div>
    <!-- stylesheetDetail -->
  </xsl:template>

  <xd:doc> Generates function summary section. Prints title of the section and
    then iterates through all functions and prints its declaration and short
    description </xd:doc>
  <xsl:template match="xsl:stylesheet" mode="functionsSummary">
    <xsl:if test="xsl:function">
      <div id="functionsSummary">
        <h2>Functions Summary</h2>
        <xsl:for-each select="xsl:function">
          <xsl:sort select="@name"/>
          <div class="listItem">
            <xsl:apply-templates mode="printDeclaration" select=".">
              <xsl:with-param name="link" select="concat('#', generate-id(.))"/>
            </xsl:apply-templates>
            <div class="shortDoc">
              <xsl:apply-templates mode="printShortDescription" select="."/>
            </div>
          </div>
        </xsl:for-each>
      </div>
    </xsl:if>
  </xsl:template>

  <xd:doc> Generates match template summary section. Prints title of the section
    and then iterates through all templates and prints its declaration and short
    description </xd:doc>
  <xsl:template match="xsl:stylesheet" mode="matchTemplatesSummary">
    <xsl:if test="xsl:template[@match]">
      <div id="matchTemplatesSummary">
        <h2>Match Templates Summary</h2>
        <xsl:for-each select="xsl:template[@match]">
          <xsl:sort select="concat(@mode, @match)"/>
          <div class="listItem">
            <xsl:apply-templates mode="printDeclaration" select=".">
              <xsl:with-param name="link" select="concat('#', generate-id(.))"/>
            </xsl:apply-templates>
            <div class="shortDoc">
              <xsl:apply-templates mode="printShortDescription" select="."/>
            </div>
          </div>
        </xsl:for-each>
      </div>
    </xsl:if>
  </xsl:template>

  <xd:doc> Generates parameter summary section. Prints title of the section and
    then iterates through all parameters and prints its declaration and short
    description </xd:doc>
  <xsl:template match="xsl:stylesheet" mode="parametersSummary">
    <xsl:if test="xsl:param">
      <div id="parametersSummary">
        <h2>Parameters Summary</h2>
        <xsl:for-each select="xsl:param">
          <xsl:sort select="@name"/>
          <div class="listItem">
            <xsl:apply-templates mode="printDeclaration" select=".">
              <xsl:with-param name="link" select="concat('#', generate-id(.))"/>
            </xsl:apply-templates>
            <div class="shortDoc">
              <xsl:apply-templates mode="printShortDescription" select="."/>
            </div>
          </div>
        </xsl:for-each>
      </div>
    </xsl:if>
  </xsl:template>

  <xd:doc> Generates template summary section. Prints title of the section and
    then iterates through all templates and prints its declaration and short
    description </xd:doc>
  <xsl:template match="xsl:stylesheet" mode="namedTemplatesSummary">
    <xsl:if test="xsl:template[@name]">
      <div id="namedTemplatesSummary">
        <h2>Named Templates Summary</h2>
        <xsl:for-each select="xsl:template[@name]">
          <xsl:sort select="@name"/>
          <div class="listItem">
            <xsl:apply-templates mode="printDeclaration" select=".">
              <xsl:with-param name="link" select="concat('#', generate-id(.))"/>
            </xsl:apply-templates>
            <div class="shortDoc">
              <xsl:apply-templates mode="printShortDescription" select="."/>
            </div>
          </div>
        </xsl:for-each>
      </div>
    </xsl:if>
  </xsl:template>

  <xd:doc> Default template in XdocTags mode. This ensures that elements that
    need no conversion(html tags) are copied to the result tree. The namespace
    of an element is translated to xhtml! </xd:doc>
  <xsl:template match="*" mode="XdocTags">
    <xsl:element name="{node-name(.)}" namespace="http://www.w3.org/1999/xhtml">
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates mode="XdocTags"/>
    </xsl:element>
  </xsl:template>

  <xd:doc> Default template in XdocTags mode for elements in xd namespace. </xd:doc>
  <xsl:template match="xd:*" mode="XdocTags">
    <xsl:apply-templates mode="XdocTags"/>
  </xsl:template>

  <xd:doc> Converts a xd:link element to a html link. Not implemented yet!</xd:doc>
  <xsl:template match="xd:link" mode="XdocTags">
    <a href="#">
      <xsl:value-of select="."/>
    </a>
  </xsl:template>
</xsl:stylesheet>
