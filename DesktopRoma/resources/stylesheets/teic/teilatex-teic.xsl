<xsl:stylesheet
 xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
 xmlns:tei="http://www.tei-c.org/ns/1.0"
 xmlns:teix="http://www.tei-c.org/ns/Examples"
 xmlns:rss="http://purl.org/rss/1.0/"
 xmlns:dc="http://purl.org/dc/elements/1.1/"
 xmlns:syn="http://purl.org/rss/1.0/modules/syndication/"
 xmlns:taxo="http://purl.org/rss/1.0/modules/taxonomy/"
 xmlns:exsl="http://exslt.org/common"
 xmlns:estr="http://exslt.org/strings"
 extension-element-prefixes="exsl estr"
 exclude-result-prefixes="exsl"
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
 version="1.0">

<xsl:import href="../latex/tei.xsl"/>
<xsl:import href="teilatex-teicdoc.xsl"/>
<xsl:output method="text" encoding="utf8"/>
<xsl:param name="institution">Text Encoding Initiative</xsl:param>
<xsl:param name="latexLogoFile">/images/oucslogo-darkblueonwhite-long.pdf</xsl:param>
<xsl:param name="latexLogoFile2">/images/oucslogo-darkblueonwhite-text2.png</xsl:param>
<xsl:param name="standardScale">0.7</xsl:param>


<xsl:template name="printTitleAndLogo">
\vskip-36pt
\setbox0=\hbox{\lower55pt\hbox{\hskip-.75in\includegraphics{logo}}}\ht0=\z@\dp0\z@\box0
\setbox0=\hbox{\parbox[b]{.75\textwidth}{\fontsize{14pt}{16pt}\bfseries\sffamily\selectfont \@title}}\ht0=\z@\dp0=\z@\box0
\vskip20pt
\par{\fontsize{11pt}{13pt}\sffamily\itshape\selectfont\@author\hfill\TheDate}
\vspace{18pt}
</xsl:template>

</xsl:stylesheet>
