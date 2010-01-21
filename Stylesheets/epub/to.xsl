<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet                 xmlns:dc="http://purl.org/dc/elements/1.1/"
                xmlns:html="http://www.w3.org/1999/xhtml"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="2.0"
                exclude-result-prefixes="tei dc html">

  <xsl:import href="../xhtml2/tei.xsl"/>
  <xsl:param name="splitLevel">0</xsl:param>
  <xsl:param name="STDOUT">false</xsl:param>
  <xsl:param name="outputDir">OEBPS</xsl:param>
  <xsl:param name="cssFile">tei.css</xsl:param>
  <xsl:param name="cssPrintFile"/>

  <xsl:template match="/">
    <xsl:apply-templates mode="split"/>

    <xsl:result-document method="text" href="OEBPS/tei.css">
      <xsl:value-of select="unparsed-text('../tei.css')"/>
    </xsl:result-document>

    <xsl:result-document method="text" href="mimetype">
      <xsl:text>application/epub+zip</xsl:text>
    </xsl:result-document>

    <xsl:result-document method="text" href="META-INF/container.xml">
      <container version="1.0" xmlns="urn:oasis:names:tc:opendocument:xmlns:container">
	<rootfiles>
	  <rootfile full-path="OEBPS/content.opf" media-type="application/oebps-package+xml"/>
	</rootfiles>
      </container>
    </xsl:result-document>

    <xsl:result-document method="text" href="OEPBPS/content.opf">
    </xsl:result-document>



    <xsl:result-document method="text" href="OEPBPS/page-template.xpgt">
      <ade:template xmlns="http://www.w3.org/1999/xhtml" xmlns:ade="http://ns.adobe.com/2006/ade"
		    xmlns:fo="http://www.w3.org/1999/XSL/Format">
	
	<fo:layout-master-set>
	  <fo:simple-page-master master-name="single_column">
	    <fo:region-body margin-bottom="3pt" margin-top="0.5em" margin-left="3pt" margin-right="3pt"/>
	  </fo:simple-page-master>
	  
	  <fo:simple-page-master master-name="single_column_head">
	    <fo:region-before extent="8.3em"/>
	    <fo:region-body margin-bottom="3pt" margin-top="6em" margin-left="3pt" margin-right="3pt"/>
	  </fo:simple-page-master>
	  
	  <fo:simple-page-master master-name="two_column"	margin-bottom="0.5em" margin-top="0.5em" margin-left="0.5em" margin-right="0.5em">
	    <fo:region-body column-count="2" column-gap="10pt"/>
	  </fo:simple-page-master>
	  
	  <fo:simple-page-master master-name="two_column_head" margin-bottom="0.5em" margin-left="0.5em" margin-right="0.5em">
	    <fo:region-before extent="8.3em"/>
	    <fo:region-body column-count="2" margin-top="6em" column-gap="10pt"/>
	  </fo:simple-page-master>
	  
	  <fo:simple-page-master master-name="three_column" margin-bottom="0.5em" margin-top="0.5em" margin-left="0.5em" margin-right="0.5em">
	    <fo:region-body column-count="3" column-gap="10pt"/>
	  </fo:simple-page-master>
	  
	  <fo:simple-page-master master-name="three_column_head" margin-bottom="0.5em" margin-top="0.5em" margin-left="0.5em" margin-right="0.5em">
	    <fo:region-before extent="8.3em"/>
	    <fo:region-body column-count="3" margin-top="6em" column-gap="10pt"/>
	  </fo:simple-page-master>
	  
	  <fo:page-sequence-master>
	    <fo:repeatable-page-master-alternatives>
	      <fo:conditional-page-master-reference master-reference="three_column_head" page-position="first" ade:min-page-width="80em"/>
	      <fo:conditional-page-master-reference master-reference="three_column" ade:min-page-width="80em"/>
	      <fo:conditional-page-master-reference master-reference="two_column_head" page-position="first" ade:min-page-width="50em"/>
	      <fo:conditional-page-master-reference master-reference="two_column" ade:min-page-width="50em"/>
	      <fo:conditional-page-master-reference master-reference="single_column_head" page-position="first" />
	      <fo:conditional-page-master-reference master-reference="single_column"/>
	    </fo:repeatable-page-master-alternatives>
	  </fo:page-sequence-master>
	  
	</fo:layout-master-set>
	
	<ade:style>
	  <ade:styling-rule selector=".title_box" display="adobe-other-region" adobe-region="xsl-region-before"/>
	</ade:style>
	
      </ade:template>      
    </xsl:result-document>


    </xsl:template>


</xsl:stylesheet>