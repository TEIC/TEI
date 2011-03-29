<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.w3.org/1999/xhtml"
                xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
		xmlns:iso="http://www.iso.org/ns/1.0"
		xmlns:cals="http://www.oasis-open.org/specs/tm9901"
                xmlns:html="http://www.w3.org/1999/xhtml"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:s="http://www.ascc.net/xml/schematron"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:t="http://www.thaiopensource.com/ns/annotations"
                xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                exclude-result-prefixes="tei html t a rng s iso tbx cals teix"
                version="2.0">
   <xsl:import href="../../../xhtml2/tei.xsl"/>
   <xsl:import href="../../../xhtml/oddprocessing.xsl"/>
   <xsl:import href="../../../odds2/teiodds.xsl"/>
   <xsl:import href="../isoutils.xsl"/>
   <xsl:import href="../isotei-schema.xsl"/>
   <xsl:import href="web.xsl"/>
   <xsl:import href="tbx.xsl"/>
   <xsl:import href="cals.xsl"/>
  
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>

         <p> This library is free software; you can redistribute it and/or
      modify it under the terms of the GNU Lesser General Public License as
      published by the Free Software Foundation; either version 2.1 of the
      License, or (at your option) any later version. This library is
      distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
      without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
      PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
      details. You should have received a copy of the GNU Lesser General Public
      License along with this library; if not, write to the Free Software
      Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA </p>
         <p>Author: See AUTHORS</p>
         <p>Id: $Id$</p>
         <p>Copyright: 2008, TEI Consortium</p>
      </desc>
   </doc>

    
   <xsl:strip-space elements="tei:bibl"/>
   <xsl:param name="numberFormat">uk</xsl:param>
   <xsl:output encoding="utf-8" omit-xml-declaration="yes" method="xhtml"
               doctype-public="-//W3C//DTD XHTML 1.0 Transitional//EN"
               doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"
               indent="yes"/>

   <xsl:param name="STDOUT">true</xsl:param>
   <xsl:param name="xhtml">true</xsl:param>
   <xsl:param name="splitLevel">-1</xsl:param>
   <xsl:param name="autoToc">true</xsl:param>
   <xsl:param name="tocDepth">3</xsl:param>
   <xsl:param name="institution">ISO</xsl:param>
   <xsl:param name="department"/>
   <xsl:param name="cssFile">http://tei.oucs.ox.ac.uk/TEIISO/iso.css</xsl:param>
   <xsl:param name="cssSecondaryFile">http://tei.oucs.ox.ac.uk/TEIISO/iso-odd.css</xsl:param>
   <xsl:param name="cssPrintFile">http://tei.oucs.ox.ac.uk/TEIISO/iso-print.css</xsl:param>
   <xsl:param name="TEIC">false</xsl:param>
   <xsl:param name="wrapLength">65</xsl:param>
   <xsl:param name="attLength">60</xsl:param>
   <xsl:param name="forceWrap">true</xsl:param>

   <xsl:template match="/">
     <xsl:variable name="All">
       <xsl:apply-templates mode="checkSchematron"/>
     </xsl:variable>
     <xsl:for-each select="$All">
      <xsl:call-template name="processTEI"/>
     </xsl:for-each>
   </xsl:template>


</xsl:stylesheet>
