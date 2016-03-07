<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xpath-default-namespace="http://www.tei-c.org/ns/1.0" 
		xmlns="http://www.tei-c.org/ns/1.0"
		xmlns:rng="http://relaxng.org/ns/structure/1.0"
		exclude-result-prefixes="#all"
		version="2.0">

<!--  <xsl:template match="datatype">
    <xsl:copy-of select="."/>
  </xsl:template>-->
  <xsl:template match="datatype">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:choose>
        <xsl:when test="rng:ref">  <dataRef key="{concat('tei',rng:ref/@name)}"/>
        </xsl:when>
        <xsl:when test="rng:data">  <dataRef name="{rng:data/@type}"/>
        </xsl:when>
        <xsl:when test="rng:text">  <textNode/>
        </xsl:when>
        
       <xsl:otherwise>
          <xsl:message>Cannot cope with datatype of </xsl:message>
	  <xsl:message><xsl:value-of select="../attDef/@ident"/>
</xsl:message>
        </xsl:otherwise>
      </xsl:choose>
      </xsl:copy>
  </xsl:template>

  <xsl:template match="content">
    <xsl:choose>
      <xsl:when test=".//rng:anyName"><xsl:copy-of select="."/></xsl:when>
      <xsl:when test=".//rng:attribute"><xsl:copy-of select="."/></xsl:when>
      <xsl:when test=".//rng:data"><xsl:copy-of select="."/></xsl:when>
      <xsl:when test=".//rng:element"><xsl:copy-of select="."/></xsl:when>
      <xsl:when test=".//rng:except"><xsl:copy-of select="."/></xsl:when>
      <xsl:when test=".//rng:name"><xsl:copy-of select="."/></xsl:when>
      <xsl:when test=".//rng:nsName"><xsl:copy-of select="."/></xsl:when>
      <xsl:when test=".//rng:param"><xsl:copy-of select="."/></xsl:when>
      <xsl:when test=".//rng:value"><xsl:copy-of select="."/></xsl:when>
      <xsl:otherwise>
	<xsl:copy>
	  <xsl:apply-templates/>
	</xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
    </xsl:template>

    <xsl:template match="rng:ref">
      <xsl:choose>
	<xsl:when test="starts-with(@name,'model.')">

		<xsl:choose>
			<xsl:when test="contains(@name,'_')">
				<classRef key="{substring-before(@name,'_')}">
					<xsl:attribute name="expand">
						<xsl:value-of select="substring-after(@name,'_')"/>
						
					</xsl:attribute>
					<xsl:call-template name="maxmin"/>
				</classRef>
			</xsl:when>
		<xsl:otherwise>
		
		<classRef key="{@name}">
			<xsl:call-template name="maxmin"/>
		</classRef>
		</xsl:otherwise>
			</xsl:choose>
	</xsl:when>
	<xsl:when test="starts-with(@name,'att.')">
	  <classRef key="{@name}">
	    <xsl:call-template name="maxmin"/>
	  </classRef>
	</xsl:when>
	<xsl:when test="starts-with(@name,'macro.')">
	  <macroRef key="{@name}">
	    <xsl:call-template name="maxmin"/>
	  </macroRef>
	</xsl:when>
	<xsl:when test="starts-with(@name,'data.')">
	  <macroRef key="{@name}">
	    <xsl:call-template name="maxmin"/>
	  </macroRef>
	</xsl:when>
	<xsl:otherwise>
	  <elementRef key="{@name}">
	    <xsl:call-template name="maxmin"/>
	  </elementRef>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:template>

    <xsl:template match="rng:group">
    	<xsl:choose><xsl:when test="count(*)>1">
    		<sequence>
	<xsl:call-template name="maxmin"/>
	<xsl:apply-templates/>
      </sequence></xsl:when>
    		<xsl:otherwise><xsl:apply-templates/>
    		</xsl:otherwise></xsl:choose>
    </xsl:template>
	
    
    <xsl:template match="rng:text">
      <textNode/>
    </xsl:template>

    <xsl:template match="rng:choice">
      <alternate>
	<xsl:call-template name="maxmin"/>
	<xsl:apply-templates/>
      </alternate>
    </xsl:template>

    <xsl:template match="rng:zeroOrMore|rng:oneOrMore|rng:optional">
      <xsl:choose>
	<xsl:when test="count(*)=1">
	  <xsl:apply-templates/>
	</xsl:when>
	<xsl:otherwise>
	  <sequence>
	    <xsl:choose>
	      <xsl:when test="self::rng:zeroOrMore">
		<xsl:attribute name="minOccurs">0</xsl:attribute>
		<xsl:attribute name="maxOccurs">unbounded</xsl:attribute>
	      </xsl:when>
	      <xsl:when test="self::rng:oneOrMore">
		<xsl:attribute name="minOccurs">1</xsl:attribute>
		<xsl:attribute name="maxOccurs">unbounded</xsl:attribute>
	      </xsl:when>
	      <xsl:when test="self::rng:optional">
		<xsl:attribute name="minOccurs">0</xsl:attribute>
	      </xsl:when>
	    </xsl:choose>
	    <xsl:apply-templates/>
	  </sequence>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:template>

    <xsl:template match="rng:empty"/>
    
    <xsl:template match="rng:*">
        <xsl:message><xsl:value-of select="local-name(.)"/><xsl:value-of select="@name"/> Unprocessed</xsl:message>

        <xsl:comment><xsl:value-of select="name(.)"/>
        <xsl:value-of select="@name"/> Unprocessed</xsl:comment>
        <xsl:apply-templates/>

    </xsl:template>

    <xsl:template
	match="rng:anyName|rng:attribute|rng:data|rng:element|rng:except|rng:name|rng:nsName|rng:param|rng:data|rng:value">
        <xsl:message><xsl:value-of select="local-name(.)"/><xsl:value-of select="@name"/> TODO</xsl:message>
      <junk>
        <xsl:apply-templates/>
      </junk>
    </xsl:template>


    <xsl:template match="exemplum">
      <xsl:copy-of select="."/>
    </xsl:template>

    <xsl:template match="*">
      <xsl:copy>
	<xsl:apply-templates select="@*|*|processing-instruction()|comment()|text()" />
      </xsl:copy>
    </xsl:template>
    
    <xsl:template match="comment()|@*|processing-instruction()|text()">
      <xsl:copy-of select="."/>
    </xsl:template>
    
    <xsl:template name="maxmin">
      <xsl:choose>
      <xsl:when test="parent::*[local-name()='zeroOrMore' or   local-name()='oneOrMore' or local-name()='optional'] and  count(../*)&gt;1"/>
      <xsl:otherwise>
	<xsl:choose>
	  <xsl:when test="parent::rng:zeroOrMore">
	    <xsl:attribute name="minOccurs">0</xsl:attribute>
	    <xsl:attribute name="maxOccurs">unbounded</xsl:attribute>
	  </xsl:when>
	  <xsl:when test="parent::rng:oneOrMore">
	    <xsl:attribute name="minOccurs">1</xsl:attribute>
	    <xsl:attribute name="maxOccurs">unbounded</xsl:attribute>
	  </xsl:when>
	  <xsl:when test="parent::rng:optional">
	    <xsl:attribute name="minOccurs">0</xsl:attribute>
	  </xsl:when>
	</xsl:choose>
      </xsl:otherwise>
      </xsl:choose>
    </xsl:template>

</xsl:stylesheet>
