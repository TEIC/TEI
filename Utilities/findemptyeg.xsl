<xsl:stylesheet 
    version="2.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    xmlns:teix="http://www.tei-c.org/ns/Examples"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    exclude-result-prefixes="tei teix">
  
  <xsl:output method="xml" indent="yes" encoding="utf-8"/>
  
  
  <xsl:key match="tei:elementSpec" use="'1'" name="E"/>
  <xsl:key match="teix:*[not(ancestor::tei:elementSpec)]" 
	   use="local-name()" name="EX"/>
  <xsl:key match="teix:*[ancestor::tei:elementSpec]" 
	   use="local-name()" name="EXEL"/>
  
  <xsl:template match="/">
    <html>
      <body>
	<table>
	  <xsl:attribute name="rules">all</xsl:attribute>
	  <xsl:attribute name="border">1</xsl:attribute>
	  <xsl:for-each select="key('E',1)">
	    <xsl:sort select="@module"/>
	    <xsl:sort select="@ident"/>
	    <xsl:if test="not(.//teix:egXML[string-length(.)&gt;3])">
	      <tr>
		<td align="top">
		  <a
		      href="http://www.tei-c.org/release/doc/tei-p5-doc/en/html/ref-{@ident}.html">
		    <xsl:value-of select="@ident"/>
		  </a>
		</td>
		<td align="top">
		  <xsl:value-of select="@module"/>
		</td>
		<td align="top">
		  <xsl:value-of  select="tei:desc"/> 
		</td>
		<td align="top">
		  <xsl:variable name="N"><xsl:value-of
		  select="count(key('EX',@ident))"/></xsl:variable>
		  <xsl:variable name="N2"><xsl:value-of
		  select="count(key('EXEL',@ident))"/></xsl:variable>
		  <xsl:choose>
		    <xsl:when test="$N=0 and $N2=0">
		      <b>There are NO examples elsewhere.</b>
		    </xsl:when>
		    <xsl:when test="$N=1 and $N2=0">
		      There is one example elsewhere in the prose.
		      <xsl:for-each select="key('EX',@ident)">
			<a>
			  <xsl:attribute name="href">
			  <xsl:text>http://www.tei-c.org/release/doc/tei-p5-doc/en/html/</xsl:text>
			  <xsl:value-of select="ancestor::tei:div[last()]/@xml:id"/>
			  <xsl:text>.html#</xsl:text>
			  <xsl:value-of select="ancestor::tei:div[@xml:id][1]/@xml:id"/>
			  </xsl:attribute>
			  <xsl:value-of select="ancestor::tei:div[1]/tei:head"/>
			</a>
		      </xsl:for-each>
		    </xsl:when>
		    <xsl:when test="$N2=1 and $N=0">
		      There is one example elsewhere in the reference material:
		      <xsl:for-each select="key('EXEL',@ident)">
			<xsl:value-of select="ancestor::tei:elementSpec/@ident"/>
		      </xsl:for-each>
		    </xsl:when>
		    <xsl:otherwise>
		      There are <xsl:value-of select="$N"/> examples
		      in the prose:
		      <ol>
		      <xsl:for-each select="key('EX',@ident)">
			<li><a>
			  <xsl:attribute name="href">
			  <xsl:text>http://www.tei-c.org/release/doc/tei-p5-doc/en/html/</xsl:text>
			  <xsl:value-of select="ancestor::tei:div[last()]/@xml:id"/>
			  <xsl:text>.html#</xsl:text>
			  <xsl:value-of select="ancestor::tei:div[@xml:id][1]/@xml:id"/>
			  </xsl:attribute>
			  <xsl:value-of select="ancestor::tei:div[1]/tei:head"/>
			</a>
			</li>
		      </xsl:for-each>
		      </ol>
		      and <xsl:value-of select="$N2"/> in the reference material.
		      <ol>
			<xsl:for-each select="key('EXEL',@ident)">
			  <li>
			    <xsl:for-each
				select="ancestor::tei:elementSpec">
			      <a
				  href="http://www.tei-c.org/release/doc/tei-p5-doc/en/html/ref-{@ident}.html">
				<xsl:value-of select="@ident"/>
			      </a>
			    </xsl:for-each>
			  </li>
			</xsl:for-each>
		      </ol>
		    </xsl:otherwise>
		  </xsl:choose>
		</td>
	      </tr>
	    </xsl:if>
	  </xsl:for-each>
	</table>
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>
