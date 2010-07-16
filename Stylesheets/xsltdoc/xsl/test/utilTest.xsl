<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:util="http://www.pnp-software.com/util" version="2.0">
	<xsl:output method="text"/>
  <xsl:include href="../lib/util.xsl"/>
  <xsl:template match="/">
    <xsl:copy-of select="util:testGetSharedPath()"/>
  </xsl:template>
  <xsl:function name="util:testGetSharedPath">
    <xsl:choose>
      <xsl:when test="util:getSharedPath('D:/test', 'D:/testCenter') = 'D:/'">
        <xsl:text>[PASSED] util:testGetSharedPath [1]</xsl:text>
        <xsl:text>&#xA;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>[FAILED] util:testGetSharedPath: [1]: </xsl:text><xsl:value-of select="util:getSharedPath('D:/test', 'D:/testCenter')"></xsl:value-of>
        <xsl:text>&#xA;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    
    <xsl:choose>
      <xsl:when test="util:getSharedPath('D:/testCenter', 'D:/test') = 'D:/'">
        <xsl:text>[PASSED] util:testGetSharedPath [2]</xsl:text>
        <xsl:text>&#xA;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>[FAILED] util:testGetSharedPath [2]: </xsl:text><xsl:value-of select="util:getSharedPath('D:/test', 'D:/testCenter')"></xsl:value-of>
        <xsl:text>&#xA;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    
    <xsl:choose>
      <xsl:when test="util:getSharedPath('D:/test/dir2', 'D:/test/dir1/dir2') = 'D:/test/'">
        <xsl:text>[PASSED] util:testGetSharedPath [3]</xsl:text>
        <xsl:text>&#xA;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>[FAILED] util:testGetSharedPath [3]: </xsl:text><xsl:value-of select="util:getSharedPath('D:/test/dir2', 'D:/test/dir1/dir2/')"></xsl:value-of>
        <xsl:text>&#xA;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    
    <xsl:choose>
      <xsl:when test="util:getSharedPath('D:/test/dir2', 'D:/test/dir2') = 'D:/test/dir2/'">
        <xsl:text>[PASSED] util:testGetSharedPath [4]</xsl:text>
        <xsl:text>&#xA;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>[FAILED] util:testGetSharedPath [4]: </xsl:text><xsl:value-of select="util:getSharedPath('D:/test/dir2', 'D:/test/dir2')"></xsl:value-of>
        <xsl:text>&#xA;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    
    <xsl:choose>
      <xsl:when test="util:getRelativeUri('file://D:/test', 'file://D:/test') = './'">
        <xsl:text>[PASSED] util:getRelativeUri [1]</xsl:text>
        <xsl:text>&#xA;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>[FAILED] util:getRelativeUri [1]: </xsl:text><xsl:value-of select="util:getRelativeUri('file://D:/test', 'file://D:/test')"></xsl:value-of>
        <xsl:text>&#xA;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
</xsl:stylesheet>