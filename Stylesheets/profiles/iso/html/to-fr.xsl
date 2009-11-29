<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.w3.org/1999/xhtml" xmlns:iso="http://www.iso.org/ns/1.0"
                xmlns:html="http://www.w3.org/1999/xhtml"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:s="http://www.ascc.net/xml/schematron"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:estr="http://exslt.org/strings"
                xmlns:t="http://www.thaiopensource.com/ns/annotations"
                xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                xmlns:edate="http://exslt.org/dates-and-times"
                xmlns:exsl="http://exslt.org/common"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                extension-element-prefixes="exsl estr edate"
                exclude-result-prefixes="exsl edate estr tei html t a rng s iso teix"
                version="1.0">
  
   <xsl:import href="to.xsl"/>
   <xsl:param name="numberFormat">fr</xsl:param>

</xsl:stylesheet>