<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:xd="http://www.pnp-software.com/XSLTdoc"
  xmlns:util="http://www.pnp-software.com/util"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  version="2.0">
  <xd:doc type="stylesheet">
    Utility functions used by other stylesheets.
    <xd:author>ibirrer</xd:author>
    <xd:cvsId>$Id: util.xsl,v 1.6 2005/01/04 10:13:06 ibirrer Exp $</xd:cvsId>
    <xd:copyright>2004, P&amp;P Software GmbH</xd:copyright>
  </xd:doc>
  <!-- ********************************************************************** -->
  <!-- ***************************** Functions ****************************** -->
  <!-- ********************************************************************** -->
  <xd:doc>Extracts the folder part of an URI.</xd:doc>
  <xsl:function name="util:getFolder">
    <xsl:param name="uri" as="xs:string"/>
    <xsl:sequence select="replace($uri, '(.*/)([^/]*)', '$1')"/>
  </xsl:function>
  
  <xd:doc>Extracts the filename of an URI.</xd:doc>
  <xsl:function name="util:getFile">
    <xsl:param name="uri" as="xs:string"/>
    <xsl:sequence select="replace($uri, '(.*/)([^/]*)', '$2')"/>
  </xsl:function>
  
  <xd:doc>Get the shared path of two folders. If no shared path is found the empty string is returned.</xd:doc>
  <xsl:function name="util:getSharedPath">
    <xsl:param name="folder1" as="xs:string"/>
    <xsl:param name="folder2" as="xs:string"/>
    
    <xsl:variable name="folder1Norm" select="util:normalizeFolder($folder1)"/>
    <xsl:variable name="folder2Norm" select="util:normalizeFolder($folder2)"/>
 
    <xsl:choose>
      <xsl:when test="$folder2Norm = ''">
        <xsl:sequence select="''"/>
      </xsl:when>
      <xsl:when test="contains($folder1Norm, $folder2Norm)">
        <xsl:sequence select="$folder2Norm"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="util:getSharedPath($folder1Norm, replace( $folder2Norm, '(.*/)([^/]*)/', '$1' ))"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>

  <xd:doc>
    <xd:short>Transforms a filesystem path to a URI. </xd:short>
    <xd:detail>
    	Backward slashes are transformed to forward slashes and the 
    	prefix <code>file:/</code> is added, if the goven path is an absoulte path. 
    	If the argument is already a URI, it is left unchanged.
    </xd:detail>
    <xd:param name="path">The Path to be transformed as a string</xd:param>
  </xd:doc>
  <xsl:function name="util:pathToUri">
    <xsl:param name="path" as="xs:string"/>
    <xsl:variable name="pathTmp" select="replace($path, '\\', '/')"/>
    <xsl:sequence select="if(contains($pathTmp, 'file:/')) then 
                            $pathTmp
                          else
                            if( util:isAbsolutePath($pathTmp) ) then
                              if (starts-with($pathTmp, '/')) then
                                concat('file:', $pathTmp)
                              else 
                                concat('file:/', $pathTmp)
                            else 
                              $pathTmp">
    </xsl:sequence>
  </xsl:function>
  
  <xd:doc> Replaces triple slashes '///' by a single slash. <xd:param
      name="uri">The uri to be normalized.</xd:param>
  </xd:doc>
  <xsl:function name="util:normalizeUri">
    <xsl:param name="uri" as="xs:string"/>
    <xsl:sequence select="replace($uri, '///', '/')"/>
  </xsl:function>
  
  <xd:doc>
    Tests if a the given path describes an absolute path.
  </xd:doc>
  <xsl:function name="util:isAbsolutePath" as="xs:boolean">
    <xsl:param name="path" as="xs:string"/>
    <xsl:sequence select="if( starts-with( $path,'/' ) or contains( $path, ':') )
                          then true()
                          else false()"/>
  </xsl:function>
  
  <xd:doc>
  	<xd:short>
  		If the uri does not end with a slash, a slash is added at the end.
  	</xd:short>
    <xd:detail>
    	Otherwise the uri is left unchanged. 
    	The result is normalized with <xd:see type="templates">normalizeUri</xd:see>
    </xd:detail>
    <xd:param name="uri">An uri that points to a folder.</xd:param>
  </xd:doc>
  <xsl:function name="util:normalizeFolder">
    <xsl:param name="uri" as="xs:string"/>
    <xsl:sequence
     select="util:normalizeUri(if (ends-with($uri, '/'))
             then $uri
             else concat($uri, '/'))"/>
  </xsl:function>
  
  <xd:doc> 
  	Returns the relative link of a given folder resolved to another folder. 
    <xd:param name="from">An absolute URI of a folder</xd:param>
    <xd:param name="to">An absolute URI of a folder</xd:param>
  </xd:doc>
  <xsl:function name="util:getRelativeUri">
    <xsl:param name="from" as="xs:string"/>
    <xsl:param name="to"   as="xs:string"/>
    <xsl:variable name="fromNorm" select="util:normalizeFolder($from)"/>
    <xsl:variable name="toNorm" select="util:normalizeFolder($to)"/>
    <xsl:variable name="sharedPath" select="util:getSharedPath($fromNorm, $toNorm)"/>
    <xsl:variable name="return">
      <xsl:choose>
        <xsl:when test="string-length( $sharedPath ) = string-length( $fromNorm )">
          <xsl:sequence select="substring( $toNorm, string-length($fromNorm) + 1 )"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:for-each select="tokenize(substring( $fromNorm, string-length($sharedPath) + 1 ), '/')">
            <xsl:if test="position() != last()">
              <xsl:text>../</xsl:text>
            </xsl:if>
          </xsl:for-each>
          <xsl:value-of select="substring( $toNorm, string-length($sharedPath) + 1 )"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
	 	<xsl:value-of select="$return"/>
  </xsl:function>
  
  <xd:doc> 
    <xd:short>Builds realive link between to files</xd:short>
    <xsl:detail>
      Like <xd:link type="xsl:function">util:getRelativeUri</xd:link>, 
      but the arguments are two abslute files URI instead of folders. The 
      filename is taken from the first argument.
    </xsl:detail>
    <xd:param name="from">An absolute URI of a file</xd:param>
    <xd:param name="to">An absolute URI of a file</xd:param>
  </xd:doc>
  <xsl:function name="util:getRelativeUriFiles">
    <xsl:param name="from" as="xs:string"/>
    <xsl:param name="to" as="xs:string"/>
    <xsl:param name="reverse" as="xs:boolean"/>
    <xsl:choose>
      <xsl:when test="not(util:isAbsolutePath($from))">
        <xsl:message terminate="yes">Error in util:getRelativeUriFiles(). $from must be an absolute URI but is: <xsl:value-of select="$from"/></xsl:message>
      </xsl:when>
      <xsl:when test="not(util:isAbsolutePath($to))">
        <xsl:message terminate="yes">Error in util:getRelativeUriFiles(). $to must be an absolute URI but is: <xsl:value-of select="$to"/></xsl:message>
      </xsl:when>
    </xsl:choose>
    
    <xsl:choose>
      <xsl:when test="$reverse">
        <xsl:value-of select="concat(util:getRelativeUri(util:getFolder($to), util:getFolder($from)), util:getFile($from))"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="concat(util:getRelativeUri(util:getFolder($from), util:getFolder($to)), util:getFile($from))"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  
  <xd:doc>
    Returns the string after the last occurence of a given character.
    If the given character is not found the text is returned without change.
    <xd:param name="text">The text from which to extarct the substring</xd:param>
    <xd:param name="token">The character after which the text should be returned</xd:param>
  </xd:doc>
  <xsl:function name="util:substringAfterLast">
    <xsl:param name="text"/>
    <xsl:param name="token"/>
    <xsl:choose>
      <xsl:when test="contains( $text, $token )">
        <xsl:variable name="regExpr" select="concat('(.*', $token,')([', $token, ']*)' )"/>
        <xsl:value-of select="replace($text, $regExpr, '$2', 's')"/>
      </xsl:when>
    </xsl:choose>
  </xsl:function>
  
  <xd:doc>
  
  </xd:doc>
  <xsl:function name="util:fileSuffixToHtml">
    <xsl:param name="fileUri"/>
    <xsl:sequence select="concat(substring-before( $fileUri, '.xml' ),'.html')"/>
  </xsl:function>
  
  <xd:doc>
    Appends an element to another element.
  </xd:doc>
  <xsl:function name="util:appendElement" as="element()">
    <xsl:param name="container" as="element()"/>
    <xsl:param name="element" as="element()"/>
    
    <xsl:element name="{node-name($container)}">
      <xsl:copy-of select="$container/@*"/>
      <xsl:copy-of select="$container/*"/>
      <xsl:copy-of select="$element"/>
    </xsl:element>
  </xsl:function>
  
  <xd:doc>
    Repeats a string several times.
    <xd:param name="text" type="string">The string to repeat</xd:param>
    <xd:param name="count" type="int">how many times should the string be repeated</xd:param>
  </xd:doc>
  <xsl:function name="util:repeatString">
    <xsl:param name="text"/>
    <xsl:param name="count"/>
    <xsl:choose>
      <xsl:when test="string-length($text) = 0 or $count &lt;= 0"/>
      <xsl:otherwise>
        <xsl:value-of select="concat($text, util:repeatString($text, $count - 1))"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>  
  
  <xd:doc>
    Strips all xml elements and comments from a set of nodes and returns only the text nodes.
  </xd:doc>
  <xsl:function name="util:stripXML">
    <xsl:param name="nodes"/>
    <xsl:apply-templates select="$nodes" mode="stripXML"/>
  </xsl:function>
  
  <xd:doc>Helper template for stripXML function. Output all text nodes</xd:doc>
  <xsl:template match="text()" mode="stripXML">
      <xsl:value-of select="."/>
  </xsl:template>
  
  <xd:doc>Helper template for stripXML function. Do not output elements, but process subelements</xd:doc>
  <xsl:template match="*" mode="stripXML">
    <xsl:apply-templates/>
  </xsl:template>
  
  <xd:doc>Helper template for stripXML function. Delete all XML comments</xd:doc>
  <xsl:template match="comment()" mode="stripXML"/>
  
  <xd:doc>
    Transforms an XML structure to a plain string.
    <xd:param name="xml">XML Element. Can also be a sequence of elements. </xd:param>
  </xd:doc>
  <xsl:function name="util:xmlToString">
    <xsl:param name="xml" as="item()*"/>
    <xsl:variable name="return">
      <xsl:apply-templates select="$xml" mode="xmlToString"/>
    </xsl:variable>
    <xsl:value-of select="string-join($return,'')"/>
  </xsl:function>
  
  <xd:doc>
    Helper template for xmlToString function.
  <xd:private/>
  </xd:doc>
  <xsl:template match="*" mode="xmlToString">
    <xsl:value-of select="concat('&lt;', name(.))"></xsl:value-of>
    <xsl:for-each select="@*">
      <xsl:value-of select="concat(name(.),'=','&quot;',. ,'&quot;',' ')"></xsl:value-of>
    </xsl:for-each>
    <xsl:value-of select="'&gt;'"></xsl:value-of>
    <xsl:apply-templates mode="xmlToString"/>
    <xsl:value-of select="concat('&lt;/', name(.),'&gt;')"></xsl:value-of>
  </xsl:template>
  
  <xsl:function name="util:xmlToHtml" as="item()*">
    <xsl:param name="xml" as="item()*"/>
    <xsl:apply-templates select="$xml[not( self::text() and (position() = 1 or position() = last()) )]" mode="xmlToHtml"/>
  </xsl:function>
  
  <xd:doc>
    Helper template for xmlToHtml function.
  <xd:private/>
  </xd:doc>
  <xsl:template match="*" mode="xmlToHtml" xmlns="http://www.w3.org/1999/xhtml" exclude-result-prefixes="#all">
    <span style="color: #990000">
      <xsl:value-of select="concat('&lt;', name(.))"/>
      <xsl:for-each select="@*">
        <xsl:value-of select="concat(' ', name(.),'=','&quot;',. ,'&quot;')"/>
      </xsl:for-each>
      <xsl:choose>
        <xsl:when test="count(node()) = 0">
          <xsl:value-of select="'/&gt;'"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="'&gt;'"/>
            <xsl:apply-templates mode="xmlToHtml"/>
          <xsl:value-of select="concat('&lt;/', name(.),'&gt;')"/>
        </xsl:otherwise>
      </xsl:choose>
      
    </span>
  </xsl:template>
  
  <xd:doc>
    Helper template for xmlToHtml function.
  <xd:private/>
  </xd:doc>
  <xsl:template match="text()" mode="xmlToHtml" xmlns="http://www.w3.org/1999/xhtml" exclude-result-prefixes="#all">
    <span style="color:black; font-weight:bold">
      <xsl:value-of select="."/>
    </span>
  </xsl:template>
  
  <xd:doc>
    Removes all indentation from each line of a text.
    <xd:param name="text" type="string">The text the indentation should be added to.</xd:param>
  </xd:doc>
  <xsl:function name="util:removeTextIndent">
    <xsl:param name="text"/>
    <!-- Use flag 'm' for multi-line mode -->
    <xsl:value-of select="replace( $text, '^\s', '', 'm' )"/>
  </xsl:function>
  
  <xd:doc>
    Removes specific indentation from each line of a text.
    <xd:param name="text" type="string">The text the indentation should removed from.</xd:param>
    <xd:param name="indent" type="string">The indentation to be removed.</xd:param>
  </xd:doc>
  <xsl:function name="util:removeTextIndent">
    <xsl:param name="text"/>
    <xsl:param name="indent"/>
    <!-- Use flag 'm' for multi-line mode -->
    <xsl:value-of select="replace( $text, concat('^', $indent), '', 'm' )"/>
  </xsl:function>
  
  <xd:doc>
    Adds indentation to each line of a text.
    <xd:param name="text" type="string">The text the indentation should be added to.</xd:param>
    <xd:param name="indent" type="string">String that is used for indentation. </xd:param>
  </xd:doc>
  <xsl:function name="util:indentText">
    <xsl:param name="text"/>
    <xsl:param name="indent"/>
    <xsl:value-of select="util:indentText( $text, $indent, true() )"/>
  </xsl:function>
  
  <xd:doc>
    Adds indentation to each line of a text.
    <xd:param name="text" type="string">The text the indentation should be added to.</xd:param>
    <xd:param name="indent" type="string">String that is used for indentation. </xd:param>
    <xd:param name="indentFirstLine" type="boolean">If true the first line is not indented </xd:param>
  </xd:doc>
  <xsl:function name="util:indentText">
    <xsl:param name="text"/>
    <xsl:param name="indent"/>
    <xsl:param name="indentFirstLine"/>
    <!-- Use flag 'm' for multi-line mode -->
    <xsl:variable name="return" select="replace( $text, '^', $indent, 'm' )"/>
    <xsl:choose>
      <xsl:when test="not($indentFirstLine)">
        <xsl:value-of select="substring( $return, string-length($indent) + 1 )"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$return"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
</xsl:stylesheet>
