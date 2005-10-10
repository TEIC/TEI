<xsl:stylesheet 
    extension-element-prefixes="exsl" 
    xmlns:xd="http://www.pnp-software.com/XSLTdoc" 
     exclude-result-prefixes="exsl XSL" 
     xmlns:exsl="http://exslt.org/common" 
     xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
     xmlns:XSL="http://www.w3.org/1999/XSL/Transform" 
     version="1.0">

<xsl:import href="/usr/share/xml/teip4/stylesheet/html/tei.xsl"/>

<xd:doc type="stylesheet">
    <xd:short>
      TEI stylesheet for making web application to set parameters
      </xd:short>
    <xd:detail>
    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
   
      </xd:detail>
    <xd:author>Sebastian Rahtz sebastian.rahtz@oucs.ox.ac.uk</xd:author>
    <xd:cvsId>$Id$</xd:cvsId>
    <xd:copyright>2005, TEI Consortium</xd:copyright>
  </xd:doc>


<xsl:include href="verbatim.xsl"/>

<xsl:key name="XDS" match="xd:doc" use="@class"/>

<xsl:param name="numberHeadings">false</xsl:param>
<xsl:output 
    encoding="iso-8859-1"
    indent="yes" 
    method="xml"/>

<xsl:template name="cgi">
<exsl:document href="stylebear" method="text" encoding="iso-8859-1">
<xsl:text>#!/usr/bin/perl&#10;</xsl:text>

<xsl:for-each select="TEI.2/text/body/div[@id]">
  <xsl:call-template name="listcgi">
    <xsl:with-param name="File">common/tei-param.xsl</xsl:with-param>
  </xsl:call-template>
  <xsl:call-template name="listcgi">
    <xsl:with-param name="File">html/tei-param.xsl</xsl:with-param>
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
my $VERSION=$query->param('Version');
my $OUTFILE=$query->param('outputFile');
print $query->header(-type=>'application/octet-stream',
		     -attachment=>$OUTFILE);
print "&lt;xsl:stylesheet \n";
print "     xmlns:tei=\"http://www.tei-c.org/ns/1.0\"\n";
print "     xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\" \n";
print "     version=\"1.0\">\n";
print "&lt;!-- XSLT stylesheet to generate HTML version of TEI document\n";
print "Written by the TEI XSL generator (Sebastian Rahtz, sebastian.rahtz\@oucs.ox.ac.uk, April 2005)\n";
print "Created on $today-->\n";
#foreach $key ($query->param) {
#              print "Key [$key] --- Value[";
#              @values = $query->param($key);
#              print join(", ",@values),"]\n";
#          }
print "&lt;xsl:import href=\"",$HOME,"/",$VERSION,"/stylesheet/html/tei.xsl\"/>\n";
foreach $key (keys %Default) {
my $Passed=$query->param($key);
my $D=$Default{$key};
$Passed =~ tr/\015//d;
if ($D eq "false") { $D=""; }
$D =~ s/^[\n\r]//;
if ($key == 'preQuote') {}
elsif ($key == 'postQuote') {}
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
</exsl:document>
</xsl:template>

<xsl:template match="/">
<xsl:call-template name="cgi"/>
<TEI.2 xmlns:html="http://www.w3.org/1999/xhtml">
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
    <revisionDesc>
      <change>
	<date>$Date$.</date>
	<respStmt>
	  <name>$Author$</name>
	</respStmt>
	<item>$Revision$</item>
      </change>
    </revisionDesc>
    </teiHeader>
    <text>
<body>
<html:form
 encoding="iso-8859-1"
 enctype="multipart/form-data"
 action="/cgi-bin/stylebear"
 method="post"
 id="stylebear">
<p>Make stylesheet for: 
    <html:select name="Version">
      <html:option value="teip4" selected="selected">TEI P4</html:option>
      <html:option value="tei" selected="selected">TEI P5</html:option>
    </html:select>
</p>
<p>Location of XSL stylesheets:
 <html:input type="textbox" name="TEIXSL" size="48"
	     value="http://www.tei-c.org/release/xml"/></p>


<p>Name of output file:
 <html:input type="textbox" name="outputFile" value="myTei.xsl"/></p>

   <xsl:for-each select="TEI.2/text/body/div[@id]">
       <p><hi><xsl:number/>: <xsl:value-of select="head"/></hi></p>
       <p><xref url="customize.xml.ID={@id}">Details of this section</xref></p>
       <table>
	 <xsl:call-template name="list">
	   <xsl:with-param
	       name="File">common/tei-param.xsl</xsl:with-param>
	 </xsl:call-template>
	 <xsl:call-template name="list">
	   <xsl:with-param
	       name="File">html/tei-param.xsl</xsl:with-param>
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
</TEI.2>
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
  <xsl:variable name="I" select="@id"/>
    <xsl:for-each select="document($File)">
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
	    <xsl:choose>
	      <xsl:when test="starts-with(xd:short,'[')">
		<xsl:value-of select="substring-after(xd:short,']')"/>
	      </xsl:when>
	      <xsl:when test="xd:short"><xsl:apply-templates
	      select="xd:short"/></xsl:when>
	      <xsl:when test="starts-with(.,'[')">
		<xsl:value-of select="substring-after(.,']')"/>
	      </xsl:when>
	      <xsl:when test="contains(.,'.')">
		<xsl:value-of select="substring-before(.,'.')"/>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:copy-of select="."/>
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:for-each>
    </xsl:for-each>
</xsl:template>

<xsl:template name="listcgi">
  <xsl:param name="File"/>
  <xsl:variable name="I" select="@id"/>
    <xsl:for-each select="document($File)">
      <xsl:for-each select="key('XDS',$I)">
	<xsl:call-template name="cgivar">
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

