<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:creativeCommons="http://backend.userland.com/creativeCommonsRssModule"
		xmlns:media="http://search.yahoo.com/mrss/"
		xmlns:itunes="http://www.itunes.com/dtds/podcast-1.0.dtd"
		xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties"
                xmlns="http://www.tei-c.org/ns/1.0"
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:iso="http://www.iso.org/ns/1.0"
                xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
                xmlns:o="urn:schemas-microsoft-com:office:office"
                xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
                xmlns:rel="http://schemas.openxmlformats.org/package/2006/relationships"
                xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
                xmlns:v="urn:schemas-microsoft-com:vml"
                xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
                xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
                xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"
                xmlns:w10="urn:schemas-microsoft-com:office:word"
                xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
                xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
                xmlns:mml="http://www.w3.org/1998/Math/MathML"
                xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
                version="2.0"
                exclude-result-prefixes="cp ve o r m v wp w10 w wne mml media
					 tbx pic rel a creativeCommons
					 itunes tei teidocx xs iso">
    
    <!-- import base conversion style -->

    <xsl:import href="../../default/docx/from.xsl"/>

    <xsl:param name="fileName"/>

    <xsl:key name="R" use="enclosure/@url" match="item"/>
    
    <xsl:template name="create-tei-header">
      <xsl:variable name="filename">
	<xsl:for-each select="document(concat($word-directory,
			      '/docProps/core.xml'),/)">
	  <xsl:value-of select="cp:coreProperties/cp:category"/>
	</xsl:for-each>
      </xsl:variable>
      <xsl:analyze-string select="$filename" 
			  regex="([^/]+)/([^/]+)/([^\.]+)\.?.*">
	<xsl:matching-substring>
	  
	  <xsl:variable name="file">
	    <xsl:text>/</xsl:text>
	    <xsl:value-of select="regex-group(3)"/>
	    <xsl:text>.</xsl:text>
	  </xsl:variable>
	  <xsl:variable name="feed">
	    <xsl:value-of select="regex-group(1)"/>
	    <xsl:text>/</xsl:text>
	    <xsl:value-of select="regex-group(2)"/>
	  </xsl:variable>
	  <xsl:variable name="lookup">
	    <xsl:text>http://rss.oucs.ox.ac.uk/oxitems/generatersstwo2.php?channel_name=</xsl:text>
	    <xsl:value-of select="$feed"/>
	    <xsl:text>&amp;destination=steeple</xsl:text>
	  </xsl:variable>
	  <xsl:for-each select="document($lookup)">
	    <xsl:choose>
	      <xsl:when
		  test="count(//item[contains(enclosure/@url,$file)])=1"> 
		<xsl:for-each
		    select="//item[contains(enclosure/@url,$file)]"> 
		  <teiHeader>
		    <fileDesc>
		      <titleStmt>
			<title>
			  <xsl:value-of
			      select="title"/>
			</title>
			  <author>
			    <xsl:for-each select="media:credit">
			      <xsl:value-of select="."/>
			      <xsl:choose>
				<xsl:when
				    test="count(following-sibling::media:credit)=1">
				  <xsl:text> and </xsl:text>
				</xsl:when>
				<xsl:when
				    test="following-sibling::media:credit">
				  <xsl:text>, </xsl:text>
				</xsl:when>
			      </xsl:choose>
			    </xsl:for-each>
			  </author>
		      </titleStmt>
		      <editionStmt>
			<edition>
			  <date>
			    <xsl:call-template
				name="convertFrom822To8601">
			      <xsl:with-param name="myDate"
					      select="pubDate"/>
			    </xsl:call-template>
			  </date>
			</edition>
		      </editionStmt>
		      <publicationStmt>
			<idno type="feed">http://rss.oucs.ox.ac.uk<xsl:value-of select="$feed"/>/rss20.xml</idno>
			<idno type="guid"><xsl:value-of select="guid"/></idno>
			<idno type="link"><xsl:value-of select="link"/></idno>
			<idno type="licence"><xsl:value-of
			select="creativeCommons:license"/></idno>
			<publisher>
			  <graphic rend="noindent" url="file:///usr/share/xml/tei/stylesheet/profiles/podcasts/docx/TranscriptHeader.jpg" width="100%"/>
			</publisher>
		      </publicationStmt>
		      <notesStmt>
			<note>
			  <xsl:value-of select="description"/>
			</note>
		      </notesStmt>
		      <sourceDesc>
			<p>
			  <xsl:value-of select="source"/>
			</p>
		      </sourceDesc>
		    </fileDesc>
		    <profileDesc>
		      <textClass>
			<keywords scheme="http://podcasts.ox.ac.uk">
			  <list>
			    <xsl:for-each
				select="tokenize(itunes:keywords,',')">
			      <item>
			      <xsl:value-of select="."/></item>
			    </xsl:for-each>
			  </list>
			</keywords>
		      </textClass>
		    </profileDesc>
		  </teiHeader>
		</xsl:for-each>
	   </xsl:when>
	   <xsl:otherwise>
	     <xsl:message terminate="yes">Error finding <xsl:value-of
	     select="$file"/> in <xsl:value-of select="$lookup"/> (from <xsl:value-of select="$filename"/>)</xsl:message>
	   </xsl:otherwise>
	 </xsl:choose>
       </xsl:for-each>
     </xsl:matching-substring>
     <xsl:non-matching-substring>
       <xsl:message terminate="yes">Error finding a valid full name in <xsl:value-of select="regex-group(0)"/></xsl:message>
     </xsl:non-matching-substring>
      </xsl:analyze-string>
    </xsl:template>
    
    <xsl:template match="tei:milestone[@unit='section']" mode="pass2"/>
    
    <xsl:template match="tei:text" mode="pass2">
      <xsl:copy>
	<xsl:for-each select="tei:body">
	  <xsl:copy>

	  <xsl:variable name="body">
	    <xsl:for-each-group select="*"
				group-starting-with="tei:p[contains(.,'START AUDIO') or
						     contains(.,'END AUDIO')]">
	      <xsl:choose>
		<xsl:when test="position()=2">
		  <xsl:for-each select="current-group()">
		    <xsl:apply-templates select="." mode="pass2"/>
		  </xsl:for-each>
		</xsl:when>
		<xsl:otherwise>
		</xsl:otherwise>
	      </xsl:choose>
	    </xsl:for-each-group>
	  </xsl:variable>
	  <xsl:for-each select="$body">
	    <xsl:for-each-group select="*"
		group-starting-with="tei:p[tei:speaker]">
		  <sp>
		    <xsl:for-each select="current-group()">
		      <xsl:copy-of select="tei:speaker"/>
		      <xsl:apply-templates select="." mode="findsp"/>
		    </xsl:for-each>
		  </sp>
	    </xsl:for-each-group>
	  </xsl:for-each>
	  </xsl:copy>
	</xsl:for-each>
      </xsl:copy>
    </xsl:template>
    
    <xsl:template match="tei:p[contains(.,'START AUDIO')]" mode="pass2"/>
    <xsl:template match="tei:hi[.='&#160;']" mode="pass2">&#160;</xsl:template>

    <xsl:template match="tei:p" mode="pass2">
      <xsl:choose>
	<xsl:when test="not(*) and string-length(.)=0"/>
	<xsl:otherwise>
	  <xsl:copy>
	    <xsl:apply-templates
		select="*|@*|processing-instruction()|comment()|text()"
		mode="pass2"/>
	  </xsl:copy>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:template>

    <xsl:template match="tei:p/text()" mode="pass2">
      <xsl:choose>
	<xsl:when test="not(preceding-sibling::node())">
	  <xsl:analyze-string select="." regex="^	?([A-z]+ ?[A-z0-9]*:)?	?(.+)">
	    <xsl:matching-substring>
	      <xsl:if test="not(regex-group(1)='')">
		<speaker>
		  <xsl:value-of select="substring-before(regex-group(1),':')"/>
		</speaker>
	      </xsl:if>
	      <xsl:for-each select="regex-group(2)">
		<xsl:call-template name="findUnclear"/>
	      </xsl:for-each>
	    </xsl:matching-substring>
	    <xsl:non-matching-substring>
	      <xsl:call-template name="findUnclear"/>
	    </xsl:non-matching-substring>
	  </xsl:analyze-string>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:call-template name="findUnclear"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:template>

    <xsl:template name="findUnclear">
      <xsl:analyze-string select="." regex="\[([^\]]+)\]">
	<xsl:matching-substring>
	  <unclear>
	    <xsl:value-of select="regex-group(1)"/>
	  </unclear>
	</xsl:matching-substring>
	<xsl:non-matching-substring>
	  <xsl:value-of select="."/>
	</xsl:non-matching-substring>
      </xsl:analyze-string>
    </xsl:template>

    
    <xsl:template match="@*|text()|comment()|processing-instruction()"
		 mode="findsp">
      <xsl:copy-of select="."/>
    </xsl:template>
    
    
    <xsl:template match="*" mode="findsp">
      <xsl:copy>
	<xsl:apply-templates 
	    select="*|@*|processing-instruction()|comment()|text()" mode="findsp"/>
      </xsl:copy>
    </xsl:template>
    

    <xsl:template match="tei:speaker" mode="findsp"/>

<xsl:template name="convertFrom822To8601">
  <xsl:param name="myDate"/>
  <xsl:variable name="date" select="substring($myDate, 6, 2)"/>
  <xsl:variable name="monthstring" select="substring($myDate, 9, 3)"/>
  <xsl:variable name="month">
    <xsl:choose>
      <xsl:when test="$monthstring='Jan'">01</xsl:when>
      <xsl:when test="$monthstring='Feb'">02</xsl:when>
      <xsl:when test="$monthstring='Mar'">03</xsl:when>
      <xsl:when test="$monthstring='Apr'">04</xsl:when>
      <xsl:when test="$monthstring='May'">05</xsl:when>
      <xsl:when test="$monthstring='Jun'">06</xsl:when>
      <xsl:when test="$monthstring='Jul'">07</xsl:when>
      <xsl:when test="$monthstring='Aug'">08</xsl:when>
      <xsl:when test="$monthstring='Sep'">09</xsl:when>
      <xsl:when test="$monthstring='Oct'">10</xsl:when>
      <xsl:when test="$monthstring='Nov'">11</xsl:when>
      <xsl:when test="$monthstring='Dec'">12</xsl:when>
      <xsl:otherwise>00</xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:variable name="year" select="substring($myDate, 13, 4)"/>
  <xsl:variable name="time" select="substring($myDate, 18, 8)"/>
  <xsl:value-of select="$year"/>
  <xsl:text>-</xsl:text>
  <xsl:value-of select="$month"/>
  <xsl:text>-</xsl:text>
  <xsl:value-of select="$date"/>
</xsl:template>

</xsl:stylesheet>
