<?xml version="1.0" ?>
<!-- 
   Schematron Text-only Report - iso_schematron_text.xsl   

   Implementation of Schematron validation that reports in text only, optionally with paths in
   a prefix form.
   
  This ISO Standard is available free as a Publicly Available Specification in PDF from ISO.
  Also see www.schematron.com for drafts and other information.

  This implementation of text is designed to run with the "Skeleton" implementation 
  of Schematron which Oliver Becker devised. The skeleton code provides a 
  Schematron implementation but with named templates for handling all output; 
  the skeleton provides basic templates for output using this API, but client
  validators can be written to import the skeleton and override the default output
  templates as required. (In order to understand this, you must understand that
  a named template such as "process-assert" in this XSLT stylesheet overrides and
  replaces any template with the same name in the imported skeleton XSLT file.)

  History:
    2007-02-09:
        * GKH Repair documentation regarding termination
        * GKH Take advantage of new stylesheetbody mode to set the output
              method to be text
    2007-02-08:
    	* RJ Add optimize parameter and update to use get-schematron-full-path-2
    	* RJ Add command-line parameter to select between the two path types
    	* RJ Validate against RNC schemas for XSLT 1 and 2 (with regex tests removed)
    2007-02-07:
        * GKH created from iso-schematron_terminator.xsl
    2007-01-19:
    	* RJ Created from iso_svrl.xslt base
     
-->
<!--
 Copyright (c) 2007 Rick Jelliffe 

 This software is provided 'as-is', without any express or implied warranty. 
 In no event will the authors be held liable for any damages arising from 
 the use of this software.

 Permission is granted to anyone to use this software for any purpose, 
 including commercial applications, and to alter it and redistribute it freely,
 subject to the following restrictions:

 1. The origin of this software must not be misrepresented; you must not claim
 that you wrote the original software. If you use this software in a product, 
 an acknowledgment in the product documentation would be appreciated but is 
 not required.

 2. Altered source versions must be plainly marked as such, and must not be 
 misrepresented as being the original software.

 3. This notice may not be removed or altered from any source distribution.
-->


<!-- The command-line parameters are:
  			phase           NMTOKEN | "#ALL" (default) Select the phase for validation
    	    diagnose=true|false    Add the diagnostics to the assertion test in reports
            generate-paths=true|false   suffix messages with ":" and the Xpath
            path-format=1|2          Which built-in path display method to use. 1 is for computers. 2 is for humans.
            message-newline=true|false  add an extra newline to the end of the message
            sch.exslt.imports semi-colon delimited string of filenames for some EXSLT implementations  
   		    optimize        "visit-no-attributes"     Use only when the schema has no attributes as the context nodes
            
-->

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns:axsl="http://www.w3.org/1999/XSL/TransformAlias"
	xmlns:sch="http://www.ascc.net/xml/schematron"
	xmlns:iso="http://purl.oclc.org/dsdl/schematron">
	<!-- Select the import statement and adjust the path as 
   necessary for your system.
	-->
	<xsl:import href="iso_schematron_skeleton.xsl"/>
	<!--
		<xsl:import href="skeleton1-5.xsl"/>
		<xsl:import href="skeleton1-6.xsl"/> -->
	<xsl:param name="diagnose">yes</xsl:param>
	<xsl:param name="phase">
		<xsl:choose>
			<!-- Handle Schematron 1.5 and 1.6 phases -->
			<xsl:when test="//sch:schema/@defaultPhase">
				<xsl:value-of select="//sch:schema/@defaultPhase"/>
			</xsl:when>
			<!-- Handle ISO Schematron phases -->
			<xsl:when test="//iso:schema/@defaultPhase">
				<xsl:value-of select="//iso:schema/@defaultPhase"/>
			</xsl:when>
			<xsl:otherwise>#ALL</xsl:otherwise>
		</xsl:choose>
	</xsl:param>
	<xsl:param name="generate-paths">true</xsl:param>
	<xsl:param name="path-format">1</xsl:param>
	<xsl:param name="message-newline">true</xsl:param>
	<xsl:param name="optimize" />
	<!-- e.g. saxon file.xml file.xsl "sch.exslt.imports=.../string.xsl;.../math.xsl" -->
	<xsl:param name="sch.exslt.imports"/>

        <xsl:template match="node()" mode="stylesheetbody">
          <xsl:comment>Importing stylesheet additions</xsl:comment>
          <axsl:output method="text"/>
          <!--create the remainder of the stylesheet-->
          <xsl:apply-imports/>
        </xsl:template>

	<!-- default output action: output all messages in simple text -->
	<xsl:template name="process-message">
		<xsl:apply-templates mode="text"/>
		<xsl:choose>
			<xsl:when test=" $generate-paths = 'true' and $path-format = '2' ">
				<axsl:text>: </axsl:text>
				<axsl:apply-templates select="." mode="schematron-get-full-path-2" />
			</xsl:when>
			<xsl:when test=" $generate-paths = 'true' ">
				<axsl:text>: </axsl:text>
				<axsl:apply-templates select="." mode="schematron-get-full-path" />
			</xsl:when>
		</xsl:choose>
		<xsl:if test=" $message-newline = 'true'" >
			<axsl:value-of select="string('&#10;')"/>
		</xsl:if>
	</xsl:template>
</xsl:stylesheet>

