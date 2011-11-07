<xsl:stylesheet    
    xmlns="http://www.tei-c.org/ns/1.0"
    xpath-default-namespace="http://www.tei-c.org/ns/1.0"
    xmlns:xd="http://www.oxygenxml.com/ns/doc/xsl"
    exclude-result-prefixes="XSL xd" 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    xmlns:XSL="http://www.w3.org/1999/XSL/Transform" 
    version="2.0">

<xsl:import href="../xhtml2/tei.xsl"/>

<doc scope="stylesheet" xmlns="http://www.oxygenxml.com/ns/doc/xsl" >
    <desc>
      <p>This software is dual-licensed:

1. Distributed under a Creative Commons Attribution-ShareAlike 3.0
Unported License http://creativecommons.org/licenses/by-sa/3.0/ 

2. http://www.opensource.org/licenses/BSD-2-Clause
		
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

* Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

This software is provided by the copyright holders and contributors
"as is" and any express or implied warranties, including, but not
limited to, the implied warranties of merchantability and fitness for
a particular purpose are disclaimed. In no event shall the copyright
holder or contributors be liable for any direct, indirect, incidental,
special, exemplary, or consequential damages (including, but not
limited to, procurement of substitute goods or services; loss of use,
data, or profits; or business interruption) however caused and on any
theory of liability, whether in contract, strict liability, or tort
(including negligence or otherwise) arising in any way out of the use
of this software, even if advised of the possibility of such damage.
</p>


    <p>$Id$</p>
    <p>Copyright 2005, TEI Consortium</p>
    </desc>
  </doc>



<xsl:key name="XDS" match="xd:doc" use="@class"/>
  <xsl:param name="startAttribute"/>
  <xsl:param name="endAttribute"/>
  <xsl:param name="startAttributeValue"/>
  <xsl:param name="endAttributeValue"/>
  <xsl:param name="startComment"/>
  <xsl:param name="endComment"/>
  <xsl:param name="startElement"/>
  <xsl:param name="endElement"/>
  <xsl:param name="startElementName"/>
  <xsl:param name="endElementName"/>
  <xsl:param name="startNamespace"/>
  <xsl:param name="endNamespace"/>
  <xsl:param name="spaceCharacter"><xsl:text>  </xsl:text></xsl:param>

<xsl:param name="numberHeadings">false</xsl:param>
<xsl:param name="numberBodyHeadings"></xsl:param>
<xsl:param name="omitNSDecls">http://www.w3.org/1999/xhtml</xsl:param>

<xsl:output 
    encoding="utf-8"
    indent="yes" 
    method="xml"/>

<xsl:template name="cgi">
<xsl:result-document href="stylebear" method="text" encoding="utf-8">
<xsl:message>Create file "stylebear" </xsl:message>
<xsl:text>#!/usr/bin/perl&#10;</xsl:text>

<xsl:for-each select="TEI/text/body/div[@xml:id]">
  <xsl:call-template name="listcgi">
    <xsl:with-param name="File">common2/tei-param.xsl</xsl:with-param>
  </xsl:call-template>
  <xsl:call-template name="listcgi">
    <xsl:with-param name="File">xhtml2/tei-param.xsl</xsl:with-param>
  </xsl:call-template>
</xsl:for-each>
<xsl:text disable-output-escaping="yes">

use CGI qw(:standard :html3);
use CGI::Carp;
use CGI::Carp qw(fatalsToBrowser);
$query = new CGI;

$today = (localtime)[3] . " " . (Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec)[(localtime)[4]] . " " . ((localtime)[5] + 1900);

&amp;Response($query) ;

sub Response {
my $HOME=$query->param('TEIXSL');
my $OUTFILE=$query->param('outputFile');
print $query->header(-type=>'application/octet-stream',
		     -attachment=>$OUTFILE);
print &lt;&lt;END;
&lt;xsl:stylesheet 
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    exclude-result-prefixes="tei xsl"
    version="2.0">
&lt;!-- XSLT stylesheet to generate HTML version of TEI document.
Written by the TEI XSL generator (Sebastian Rahtz, sebastian.rahtz\@oucs.ox.ac.uk)
Created on $today-->
END
#foreach $key ($query->param) {
#              print "Key [$key] --- Value[";
#              @values = $query->param($key);
#              print join(", ",@values),"]\n";
#          }
print "&lt;xsl:import href=\"";
print $HOME;
print "/tei/stylesheet/xhtml2/tei.xsl\"/>\n";
foreach $key (keys %Default) {
my $Passed=$query->param($key);
my $D=$Default{$key};
$Passed =~ tr/\015//d;
if ($D eq "false") { $D=""; }
$D =~ s/^[\n\r]//;
if ($key eq 'preQuote') {}
elsif ($key eq 'postQuote') {}
elsif ($Passed ne $D) {
# print "&lt;!--\n\DIFFERENCE in $key:|$Passed|$D|--&gt;\n";
 print qq(&lt;xsl:$Type{$key} name="$key">);
 print "$Passed";
 print "&lt;/xsl:$Type{$key}>\n";
}
}
print "&lt;/xsl:stylesheet>\n";

return "";
}
</xsl:text>
</xsl:result-document>
</xsl:template>

<xsl:template match="/">
<xsl:call-template name="cgi"/>
<TEI xmlns="http://www.tei-c.org/ns/1.0" xmlns:html="http://www.w3.org/1999/xhtml">
    <teiHeader>
      <fileDesc>
	<titleStmt>
	  <title>The Stylebear: XSL stylesheet maker</title>
	  <author>Sebastian Rahtz</author>
	</titleStmt>
	<publicationStmt>
	  <p> </p>
	</publicationStmt>
	<sourceDesc>
	  <p></p>
	</sourceDesc>
    </fileDesc>
    </teiHeader>
    <text>
<body>
<html:form
 encoding="utf-8"
 enctype="multipart/form-data"
 action="/cgi-bin/stylebear"
 method="post"
 id="stylebear">
<p>Location of XSL stylesheets:
 <html:input type="textbox" name="TEIXSL" size="48"
	     value="http://www.tei-c.org/release/xml"/></p>


<p>Name of output file:
 <html:input type="textbox" name="outputFile" value="myTei.xsl"/></p>

   <xsl:for-each select="TEI/text/body/div[@xml:id]">
       <p><hi><xsl:number/>: <xsl:value-of select="head"/></hi></p>
       <p><ref target="customize.html#{@xml:id}">Details of this section</ref></p>
       <table>
	 <xsl:call-template name="list">
	   <xsl:with-param
	       name="File">common2/tei-param.xsl</xsl:with-param>
	 </xsl:call-template>
	 <xsl:call-template name="list">
	   <xsl:with-param
	       name="File">xhtml2/tei-param.xsl</xsl:with-param>
	 </xsl:call-template>
       </table>
   </xsl:for-each>

<p><hi>Time to work!</hi></p>
   <list>
     <item><html:input value="Submit" type="submit"/></item>
     <item><html:input value="Reset" type="reset"/></item>
   </list>
</html:form>
</body>
</text>
</TEI>
</xsl:template>

<xsl:template name="var">
<xsl:param name="default"/>
<xsl:param name="type"/>
<xsl:param name="name"/>
<xsl:param name="description"/>
<row><cell><hi><xsl:value-of select="$name"/></hi></cell>
 <xsl:variable name="length">
  <xsl:value-of select="string-length($default)"/>
 </xsl:variable>
  <xsl:variable name="what">
   <xsl:choose>
   <xsl:when test="contains($default,'&#34;')">textarea</xsl:when>
   <xsl:when test="$type=''">textarea</xsl:when>
   <xsl:when test="$length > 50">textarea</xsl:when>
   <xsl:otherwise>input</xsl:otherwise>
   </xsl:choose>
  </xsl:variable>
  <cell>  <xsl:element name="{$what}" xmlns="http://www.w3.org/1999/xhtml">
  <xsl:attribute name="name"><xsl:value-of select="$name"/></xsl:attribute>
   <xsl:choose>
     <xsl:when test="@type='boolean'">
      <xsl:attribute name="type">checkbox</xsl:attribute>
      <xsl:if test="$default='true'">
        <xsl:attribute name="checked">checked</xsl:attribute>
      </xsl:if>
      <xsl:attribute name="value">true</xsl:attribute>
     </xsl:when>
     <xsl:when test="$what='textarea'">
        <xsl:attribute name="type">textarea</xsl:attribute>
     </xsl:when>
     <xsl:otherwise>
       <xsl:attribute name="type">textbox</xsl:attribute>
       <xsl:attribute name="value">
	 <xsl:value-of select="$default"/>
       </xsl:attribute>
     </xsl:otherwise>
   </xsl:choose>
   <xsl:choose>
   <xsl:when test="$type='integer'">
      <xsl:attribute name="size">4</xsl:attribute>
   </xsl:when>
   <xsl:when test="$type=''">
      <xsl:attribute name="cols">40</xsl:attribute>
      <xsl:attribute name="rows">5</xsl:attribute>
   </xsl:when>
   <xsl:when test="$what='textarea'">
      <xsl:attribute name="cols">40</xsl:attribute>
      <xsl:attribute name="rows">5</xsl:attribute>
   </xsl:when>
   <xsl:when test="$length =0  and $type='string'">
      <xsl:attribute name="size">12</xsl:attribute>
   </xsl:when>
   <xsl:when test="$type='string'">
      <xsl:attribute name="size"><xsl:value-of select="$length + 4"/></xsl:attribute>
   </xsl:when>
   <xsl:when test="@type='anyURI'">
      <xsl:attribute name="size"><xsl:value-of select="$length + 4"/></xsl:attribute>
   </xsl:when>
   </xsl:choose>
   <xsl:if test="$what='textarea'">
     <xsl:value-of select="$default"/>
   </xsl:if>

   </xsl:element>
</cell>
<cell><xsl:value-of select="$description"/></cell>
</row>
</xsl:template>

<xsl:template match="default">
 <xsl:choose>
   <xsl:when test="contains(.,'@')">
     <xsl:value-of  disable-output-escaping="yes" select="substring-before(.,'@')"/>
     <xsl:value-of select="'\@'"/>
     <xsl:value-of  disable-output-escaping="yes" select="substring-after(.,'@')"/>
   </xsl:when>
   <xsl:otherwise>
     <xsl:value-of  disable-output-escaping="yes" select="."/>
   </xsl:otherwise>
 </xsl:choose>
</xsl:template>

<xsl:template name="list">
  <xsl:param name="File"/>
  <xsl:variable name="I" select="@xml:id"/>
    <xsl:for-each select="document(concat('../',$File))">
      <xsl:for-each select="key('XDS',$I)">
	<xsl:call-template name="var">
	  <xsl:with-param name="default">
	    <xsl:for-each select="following-sibling::xsl:*[1]">
	      <xsl:choose>
		<xsl:when test="*">
		  <xsl:apply-templates select="*|text()"
				       mode="verbatim"/>
		</xsl:when>
		<xsl:otherwise>
		<xsl:value-of select="."/>
		</xsl:otherwise>
	      </xsl:choose>
	    </xsl:for-each>
	  </xsl:with-param>
	  <xsl:with-param name="type">
	    <xsl:value-of select="@type"/>
	  </xsl:with-param>
	  <xsl:with-param name="name">
	    <xsl:value-of select="following-sibling::xsl:*[1]/@name"/>
	  </xsl:with-param>
	  <xsl:with-param name="description">
	    <xsl:apply-templates select="xd:desc"/>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:for-each>
    </xsl:for-each>
</xsl:template>

<xsl:template name="listcgi">
  <xsl:param name="File"/>
  <xsl:variable name="I" select="@xml:id"/>
    <xsl:for-each select="document(concat('../',$File))">
      <xsl:for-each select="key('XDS',$I)">
	<xsl:call-template name="cgivar">
	  <xsl:with-param name="default">
	    <xsl:for-each select="following-sibling::xsl:*[1]">
	      <xsl:choose>
		<xsl:when test="*">
		  <xsl:apply-templates select="*|text()"			       mode="verbatim"/>
		</xsl:when>
		<xsl:otherwise>
		<xsl:value-of select="."/>
		</xsl:otherwise>
	      </xsl:choose>
	    </xsl:for-each>
	  </xsl:with-param>
	  <xsl:with-param name="type">
	    <xsl:value-of select="local-name(following-sibling::xsl:*[1])"/>
	  </xsl:with-param>
	  <xsl:with-param name="name">
	    <xsl:value-of select="following-sibling::xsl:*[1]/@name"/>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:for-each>
    </xsl:for-each>
</xsl:template>


<xsl:template name="cgivar">
<xsl:param name="name"/>
<xsl:param name="type"/>
<xsl:param name="default"/>
 <xsl:text  disable-output-escaping="yes">
$Type{"</xsl:text>
 <xsl:value-of select="$name"/>
 <xsl:text>"}="</xsl:text>
 <xsl:value-of select="$type"/>
 <xsl:text>";
</xsl:text>
 <xsl:text  disable-output-escaping="yes">$Default{"</xsl:text>
 <xsl:value-of select="$name"/>
 <xsl:text>"}=q(</xsl:text>
 <xsl:value-of select="$default"/>
 <xsl:text>);
</xsl:text>
</xsl:template>

</xsl:stylesheet>

