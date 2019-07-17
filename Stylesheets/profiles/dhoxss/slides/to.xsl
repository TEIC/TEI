<xsl:stylesheet
    version="2.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  exclude-result-prefixes="tei teix xsl">
  
<xsl:import href="../../../slides/teilatex-slides.xsl"/>
<xsl:param name="latexLogo"></xsl:param>
<xsl:param name="logoFile"></xsl:param>
<xsl:param name="spaceCharacter">\hspace*{6pt}</xsl:param>
<xsl:param name="beamerClass">Singapore</xsl:param>
<xsl:param name="showNamespaceDecls">false</xsl:param>

  <xsl:param name="omitNSDecls">
    xmlns="http://purl.org/NET/crm-owl#
    xmlns="http://www.w3.org/1999/02/22-rdf-syntax-ns#
    http://www.tei-c.org/ns/1.0
    http://www.w3.org/1999/xhtml
  </xsl:param>
  <xsl:template name="latexPreambleHook">
\usepackage{pdfpages}
  </xsl:template>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[html] show an XML element in a verbatim context</desc>
  </doc>


<xsl:template name="latexPackages">
%\setmonofont{Junicode}
%\setmonofont[Scale=0.86]{Lucida Sans Typewriter}
\setmonofont[Scale=0.8]{DejaVu Sans Mono}
%\setromanfont{Minion Pro}
\setsansfont{Myriad Pro}
\usetheme{<xsl:value-of select="$beamerClass"/>}
\useinnertheme[shadow=true]{rounded}
\usecolortheme{beaver}
\setbeamercolor*{frametitle}{parent=palette primary}
\usepackage{fancyvrb,fancyhdr,longtable,framed}
\definecolor{shadecolor}{gray}{0.95}
\def\Gin@extensions{.pdf,.png,.jpg,.mps,.tif}
\setbeamercovered{transparent}
\usenavigationsymbolstemplate{}
\xdefinecolor{blue1}{rgb}{0, 0, 0.7}
\xdefinecolor{blue2}{rgb}{0, 0, 1}
\let\mainmatter\relax
\let\frontmatter\relax
\let\backmatter\relax
\let\endfoot\relax
\let\endlastfoot\relax
\parskip3pt
\setbeamertemplate{footline}
{\hspace{1em}\includegraphics[height=10ex]{<xsl:value-of select="$logoFile"/>} \hspace{2em}
\textcolor{blue1}{@dhoxss \#dhoxss} \hfill \textcolor{gray}{\insertframenumber/\inserttotalframenumber}}
</xsl:template>

<xsl:template match="tei:gi">
  <xsl:text>{\color{blue2}&lt;</xsl:text>
  <xsl:value-of select="."/>
  <xsl:text>&gt;}</xsl:text>
</xsl:template>


  <xsl:template name="Element">
    <xsl:param name="content"/>
    <xsl:text>{\color{blue1}</xsl:text>
      <xsl:copy-of select="$content"/>
    <xsl:text>}</xsl:text>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[latex] show an XML element name in a verbatim context</desc>
  </doc>
  <xsl:template name="ElementName">
    <xsl:param name="content"/>
    <xsl:text>\textbf{\color{blue1}</xsl:text>
      <xsl:copy-of select="$content"/>
    <xsl:text>}</xsl:text>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[latex] show an XML attribute value in a verbatim context</desc>
  </doc>

  <xsl:template name="AttributeValue">
    <xsl:param name="content"/>
    <xsl:text>{\color{blue2}</xsl:text>
      <xsl:copy-of select="$content"/>
    <xsl:text>}</xsl:text>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[latex] show an XML attribute in a verbatim context</desc>
  </doc>

  <xsl:template name="Attribute">
    <xsl:param name="content"/>
    <xsl:text>{\color{blue2}</xsl:text>
      <xsl:copy-of select="$content"/>
    <xsl:text>}</xsl:text>
  </xsl:template>
  

</xsl:stylesheet>


