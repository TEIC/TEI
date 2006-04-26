<?xml version="1.0" encoding="UTF-8"?>
<XSLTdocConfig>
  <Title>TEI Stylesheet family</Title>
  <Introduction>
    <p>This is a set of XSLT specifications to transform TEI XML documents to
HTML, and to XSL Formatting Objects. These web pages
provides technical documentation for the XSL, using XSLTdoc;
if you want an overview, look at the <a href="../index.xml">general
introduction</a>  and if you want to understand customization, look
at the <a href="../customize.xml">Customization Handbook</a>.</p>
    <p>The <xref url="style.xml">Stylebear</xref> web form
will construct a XSL file for you, with all the variables configured.
</p>
  </Introduction>
  <TargetDirectory path="./xsltdoc/"/>
  <SourceDirectory path="../"/>
  <RootStylesheets>
    <File href="html/tei.xsl"/>
    <File href="fo/tei.xsl"/>
    <File href="latex/tei.xsl"/>
  </RootStylesheets>
<!--
  <AdditionalCSS>
    <File href="http://localhost/stylesheet/teic.css"/>
  </AdditionalCSS>
-->
</XSLTdocConfig>
