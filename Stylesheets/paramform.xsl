<xsl:stylesheet 
     xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
     version="1.0">
<xsl:import href="http://www.oucs.ox.ac.uk/stylesheets/teihtml.xsl"/>
<xsl:variable name="masterFile"></xsl:variable>
<xsl:variable name="feedbackURL">mailto:feedback\@oucs.ox.ac.uk</xsl:variable>
<xsl:output 
    indent="yes" 
    omit-xml-declaration="yes"
    method="xml" 
/>


<xsl:template match="/">
<xsl:text>#!/usr/bin/perl
</xsl:text>
<xsl:for-each select=".//var">
 <xsl:text  disable-output-escaping="yes">
$Type{"</xsl:text>
 <xsl:value-of select="@name"/>
 <xsl:text>"}="</xsl:text>
 <xsl:value-of select="@method"/>
 <xsl:text>";
</xsl:text>
 <xsl:text  disable-output-escaping="yes">$Default{"</xsl:text>
 <xsl:value-of select="@name"/>
 <xsl:text>"}=qq(</xsl:text>
 <xsl:apply-templates select="default"/>
 <xsl:text>);
</xsl:text>
</xsl:for-each>
<xsl:text disable-output-escaping="yes">

use CGI qw(:standard :html3);
use CGI::Carp;
use CGI::Carp qw(fatalsToBrowser);

$query = new CGI;

$today = (localtime)[3] . " " . (Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec)[(localtime)[4]] . " " . ((localtime)[5] + 1900);

$path_info = $query->path_info;

if ($path_info eq '/mytei.xsl') {
  &amp;Response($query) ;
}
else
  { 
 &amp;Form($query->script_name);
}

sub Response {
$HOME=$query->param('teixslHome');
print "Content-type: text/xsl\n\n";
print "&lt;xsl:stylesheet \n";
print "     xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\" \n";
print "     version=\"1.0\">\n";
print "&lt;!-- XSLT stylesheet to generate HTML version of TEI document\n";
print "Written by the TEI XSL generator (Sebastian Rahtz, sebastian.rahtz\@oucs.ox.ac.uk, October 2000)\n";
print "Created on $today-->\n";
print "&lt;xsl:import href=\"",$HOME,"teihtml.xsl\"/>\n";
foreach $key (keys %Default) {
$Passed=$query->param($key);
$Passed =~ tr/\015//d;
#print "$key: $Passed: $Default{$key}\n";
if ($Passed ne $Default{$key}) {
 if ($Passed eq "false") { $Passed=""; }
 print "&lt;xsl:$Type{$key} name=\"$key\">";
 print $Passed;
 print "&lt;/xsl:$Type{$key}>\n";
}
}
print "&lt;/xsl:stylesheet>\n";

return "";
}

sub Form {
my ($ME)=@_;
print $query->header;

print &lt;&lt;EOFORM1;
</xsl:text>
<xsl:apply-templates/>

EOFORM1

return "";
}</xsl:template>

<xsl:template match="body">
      <form
	 method="POST" name="PizzaForm"
         enctype="multipart/form-data"
         action="$ME/mytei.xsl"
         >
   <xsl:apply-templates/>
<h3>Time to work!</h3>
<ul>
<li><input value="Submit" type="submit"/></li>
<li><input value="Reset" type="reset"/></li>
</ul>

 </form>
</xsl:template>

<xsl:template match="var">
<xsl:text disable-output-escaping="yes">&lt;br&gt;</xsl:text>
 <xsl:variable name="length">
  <xsl:value-of select="string-length(default)"/>
 </xsl:variable>
  <xsl:variable name="what">
   <xsl:choose>
   <xsl:when test="$length > 50">textarea</xsl:when>
   <xsl:otherwise>input</xsl:otherwise>
   </xsl:choose>
  </xsl:variable>
  <xsl:element name="{$what}">
  <xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
   <xsl:choose>
     <xsl:when test="@type='boolean'">
      <xsl:attribute name="type">checkbox</xsl:attribute>
      <xsl:if test="default='true'">
        <xsl:attribute name="checked">checked</xsl:attribute>
      </xsl:if>
      <xsl:attribute name="value">true</xsl:attribute>
     </xsl:when>
     <xsl:when test="$length > 50">
        <xsl:attribute name="type">textarea</xsl:attribute>
     </xsl:when>
     <xsl:otherwise>
      <xsl:attribute name="type">textbox</xsl:attribute>
      <xsl:attribute name="value">
       <xsl:apply-templates select="default"/>
      </xsl:attribute>
     </xsl:otherwise>
   </xsl:choose>
   <xsl:choose>
   <xsl:when test="@type='number'">
      <xsl:attribute name="size">4</xsl:attribute>
   </xsl:when>
   <xsl:when test="$length > 50 and @type='text'">
      <xsl:attribute name="cols">40</xsl:attribute>
      <xsl:attribute name="rows">5</xsl:attribute>
   </xsl:when>
   <xsl:when test="$length =0  and @type='text'">
      <xsl:attribute name="size">12</xsl:attribute>
   </xsl:when>
   <xsl:when test="@type='text'">
      <xsl:attribute name="size"><xsl:value-of select="$length + 4"/></xsl:attribute>
   </xsl:when>
   <xsl:when test="@type='url'">
      <xsl:attribute name="size"><xsl:value-of select="$length + 4"/></xsl:attribute>
   </xsl:when>
   </xsl:choose>
   <xsl:if test="$length > 50">
      <xsl:value-of select="default"/>
   </xsl:if>
   </xsl:element>
<xsl:text> </xsl:text>
<em><xsl:value-of select="description"/></em>
<xsl:text disable-output-escaping="yes">&lt;br&gt;</xsl:text>
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
</xsl:stylesheet>

