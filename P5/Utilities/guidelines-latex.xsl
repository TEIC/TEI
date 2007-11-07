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
  
<xsl:import href="/usr/share/xml/tei/stylesheet/latex/tei.xsl"/>
<xsl:param name="reencode">false</xsl:param>
<xsl:param name="numberBackHeadings">false</xsl:param>
<xsl:param name="spaceCharacter">\hspace*{1em}</xsl:param>
<xsl:param name="classParameters">11pt,twoside</xsl:param>
  <xsl:variable name="docClass">book</xsl:variable>
<xsl:template name="latexPreambleHook">
\usepackage{framed}
\definecolor{shadecolor}{gray}{0.9}
\setromanfont{Gentium}
\setsansfont[Scale=0.9]{Lucida Sans}
\setmonofont[Scale=0.9]{Lucida Sans Typewriter}
\setlength{\headheight}{14pt}
</xsl:template>


<xsl:template name="latexBegin">
\makeatletter
\thispagestyle{plain}
<xsl:if test="not(tei:text/tei:front/tei:titlePage)">
  <xsl:call-template name="printTitleAndLogo"/>
</xsl:if>
\markright{\@title}%
\markboth{\@title}{\@author}%
\makeatother
\fvset{frame=single,numberblanklines=false,xleftmargin=5mm,xrightmargin=5mm}
\fancyhf{} 
\setlength{\headheight}{14pt}
\fancyhead[LE]{\bfseries\leftmark} 
\fancyhead[RO]{\bfseries\rightmark} 
\fancyfoot[RO]{\TheDate}
\fancyfoot[CO]{\thepage}
\fancyfoot[LO]{}
\fancyfoot[LE]{\TheDate}
\fancyfoot[CE]{\thepage}
\fancyfoot[RE]{}
\fancypagestyle{plain}{\fancyhead{}\renewcommand{\headrulewidth}{0pt}}
<xsl:call-template name="beginDocumentHook"/>
</xsl:template>

<xsl:param name="latexGeometryOptions">twoside,letterpaper,lmargin=.8in,rmargin=.8in,tmargin=.8in,bmargin=.8in</xsl:param>

<xsl:template match="tei:byline"/>
<xsl:template match="tei:titlePage/tei:note"/>

  <xsl:template match="tei:titlePage">
  \begin{titlepage}
  \fontsize{30pt}{36pt}\bfseries\textsf\selectfont
  <xsl:apply-templates/>
  \maketitle
  \end{titlepage}
  \cleardoublepage
</xsl:template>

<xsl:template match="tei:list">
  <xsl:if test="parent::tei:item">\mbox{}\\[-10pt] </xsl:if>
  <xsl:apply-imports/>
</xsl:template>

  <xsl:template name="lineBreak">
    <xsl:param name="id"/>
    <xsl:text>\mbox{}\newline &#10;</xsl:text>
  </xsl:template>

  <xsl:template name="graphicsAttributes">
    <xsl:param name="mode">fo</xsl:param>
    <xsl:if test="@width">
      <xsl:choose>
        <xsl:when test="contains(@width,'%')">
          <xsl:choose>
            <xsl:when test="$mode='fo'">
              <xsl:attribute name="content-width">
                <xsl:value-of select="@width"/>
              </xsl:attribute>
            </xsl:when>
            <xsl:when test="$mode='latex'">
              <xsl:text>width=</xsl:text>
              <xsl:value-of select="substring-before(@width,'%') div 100"/>
              <xsl:text>\textwidth,</xsl:text>
            </xsl:when>
            <xsl:otherwise>
              <xsl:attribute name="width">
                <xsl:value-of select="@width"/>
              </xsl:attribute>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:when>
        <xsl:otherwise>
          <xsl:variable name="w">
            <xsl:choose>
              <xsl:when test="contains(@width,'pt')">
                <xsl:value-of select="@width"/>
              </xsl:when>
              <xsl:when test="contains(@width,'px') and $mode='latex'">
                <xsl:value-of select="substring-before(@width,'px')"/>
                <xsl:text>pt</xsl:text>
              </xsl:when>
              <xsl:when test="contains(@width,'in')">
                <xsl:value-of select="@width"/>
              </xsl:when>
              <xsl:when test="contains(@width,'cm')">
                <xsl:value-of select="@width"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="@width"/>
                <xsl:text>pt</xsl:text>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:variable>
          <xsl:choose>
            <xsl:when test="$mode='fo'">
              <xsl:attribute name="content-width">
                <xsl:value-of select="$w"/>
              </xsl:attribute>
            </xsl:when>
            <xsl:when test="$mode='latex'">
              <xsl:text>width=</xsl:text>
              <xsl:value-of select="$w"/>
              <xsl:text>,</xsl:text>
            </xsl:when>
          </xsl:choose>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:if>
    <xsl:if test="@height">
      <xsl:choose>
        <xsl:when test="contains(@height,'%')">
          <xsl:choose>
            <xsl:when test="$mode='fo'">
              <xsl:attribute name="content-height">
                <xsl:value-of select="@height"/>
              </xsl:attribute>
            </xsl:when>
            <xsl:when test="$mode='latex'">
              <xsl:text>height=</xsl:text>
              <xsl:value-of select="substring-before(@height,'%') div 100"/>
              <xsl:text>\textheight,</xsl:text>
            </xsl:when>
            <xsl:otherwise>
              <xsl:attribute name="height">
                <xsl:value-of select="@height"/>
              </xsl:attribute>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:when>
        <xsl:otherwise>
          <xsl:variable name="h">
            <xsl:choose>
              <xsl:when test="contains(@height,'pt')">
                <xsl:value-of select="@height"/>
              </xsl:when>
              <xsl:when test="contains(@height,'px') and $mode='latex'">
                <xsl:value-of select="substring-before(@height,'px')"/>
                <xsl:text>pt</xsl:text>
              </xsl:when>
              <xsl:when test="contains(@height,'in')">
                <xsl:value-of select="@height"/>
              </xsl:when>
              <xsl:when test="contains(@height,'cm')">
                <xsl:value-of select="@height"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="@height"/>
                <xsl:text>pt</xsl:text>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:variable>
          <xsl:choose>
            <xsl:when test="$mode='fo'">
              <xsl:attribute name="content-height">
                <xsl:value-of select="$h"/>
              </xsl:attribute>
            </xsl:when>
            <xsl:when test="$mode='latex'">
              <xsl:text>height=</xsl:text>
              <xsl:value-of select="$h"/>
              <xsl:text>,</xsl:text>
            </xsl:when>
          </xsl:choose>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:if>
    <xsl:variable name="s">
      <xsl:choose>
        <xsl:when test="@scale and contains(@scale,'%')">
          <xsl:value-of select="substring-before(@scale,'%') div 100"/>
        </xsl:when>
        <xsl:when test="@scale">
          <xsl:value-of select="@scale"/>
        </xsl:when>
        <xsl:when test="not(@width) and not(@height) and not($standardScale=1)">
          <xsl:value-of select="$standardScale"/>
        </xsl:when>
      </xsl:choose>
    </xsl:variable>
    <xsl:if test="not($s='')">
      <xsl:choose>
        <xsl:when test="$mode='fo'">
          <xsl:attribute name="scale">
            <xsl:value-of select="$s"/>
          </xsl:attribute>
        </xsl:when>
        <xsl:when test="$mode='latex'">
          <xsl:text>scale=</xsl:text>
          <xsl:value-of select="$s"/>
          <xsl:text>,</xsl:text>
        </xsl:when>
      </xsl:choose>
    </xsl:if>
  </xsl:template>

  <xsl:template name="makePic">
    <xsl:if test="@xml:id">\hypertarget{<xsl:value-of select="@xml:id"/>}{}</xsl:if>
    <xsl:if test="@rend='centre'">
      <xsl:text>\centerline{</xsl:text>
    </xsl:if>
    <xsl:text>\includegraphics[</xsl:text>
    <xsl:call-template name="graphicsAttributes">
      <xsl:with-param name="mode">latex</xsl:with-param>
    </xsl:call-template>
    <xsl:text>]{</xsl:text>
    <xsl:choose>
      <xsl:when test="$realFigures='true'">
        <xsl:choose>
          <xsl:when test="@url">
            <xsl:value-of select="substring-before(@url,'.')"/>
          </xsl:when>
          <xsl:when test="@entity">
            <xsl:value-of select="unparsed-entity-uri(@entity)"/>
          </xsl:when>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="c">
          <xsl:for-each select="ancestor-or-self::tei:figure[1]">
            <xsl:number level="any"/>
          </xsl:for-each>
        </xsl:variable>
        <xsl:text>FIG</xsl:text>
        <xsl:value-of select="$c+1000"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>}</xsl:text>
    <xsl:if test="@rend='centre'">
      <xsl:text>}</xsl:text>
    </xsl:if>
  </xsl:template>

</xsl:stylesheet>


