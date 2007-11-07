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
\setromanfont{Lucida Bright}
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

<xsl:template match="tei:list">
  <xsl:if test="parent::tei:item">\mbox{}\\[-10pt] </xsl:if>
  <xsl:apply-imports/>
</xsl:template>

  <xsl:template name="lineBreak">
    <xsl:param name="id"/>
    <xsl:text>\mbox{}\newline &#10;</xsl:text>
  </xsl:template>


</xsl:stylesheet>


