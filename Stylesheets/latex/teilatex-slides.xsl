<!-- 
TEI XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL FO stylesheet to format TEI XML documents 

#include LICENSE
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:import href="teilatex-lib.xsl"/>

<xsl:output method="text"/>

<xsl:template match="tei:TEI">
\documentclass[a4paper]{article}
\usepackage{amsmath}
\usepackage[contnav]{pdfslide}
\pagestyle{title}
\usepackage{xmllatex}
\begin{document}
\orgurl{\protect\color{white}http://www.oucs.ox.ac.uk/}
\orgname{Oxford University Computing Services}
\author{<xsl:apply-templates select="/tei:TEI//tei:front//tei:docAuthor"/>}
\title{<xsl:value-of select="/tei:TEI//tei:front//tei:titlePart[@type='main']"/>}
\date{<xsl:value-of select="/tei:TEI//tei:front//tei:docDate"/>}
\pagedissolve{Wipe /D 1 /Di /H /M /O}
\color{section0}
\overlay{d12.jpg}
\maketitle
\overlay{metablue.pdf}

<xsl:apply-templates select="tei:text/tei:body"/>
\end{document}
</xsl:template>


<xsl:template match="tei:div/tei:head">
\section{<xsl:apply-templates/>}
</xsl:template>

<xsl:template match="tei:div/tei:div/tei:head">
\subsection{<xsl:apply-templates/>}
</xsl:template>

</xsl:stylesheet>
