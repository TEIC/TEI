<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet
 xmlns="http://www.tei-c.org/ns/1.0"
 xmlns:w="http://schemas.microsoft.com/office/word/2003/wordml" 
 xmlns:h="http://www.w3.org/1999/xhtml"
 xmlns:v="urn:schemas-microsoft-com:vml" 
 xmlns:w10="urn:schemas-microsoft-com:office:word" 
 xmlns:sl="http://schemas.microsoft.com/schemaLibrary/2003/core" 
 xmlns:aml="http://schemas.microsoft.com/aml/2001/core" 
 xmlns:wx="http://schemas.microsoft.com/office/word/2003/auxHint" 
 xmlns:o="urn:schemas-microsoft-com:office:office" 
 xmlns:dt="uuid:C2F41010-65B3-11d1-A29F-00AA00C14882" 
 xmlns:st1="urn:schemas-microsoft-com:office:smarttags"
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 xmlns:tei="http://www.tei-c.org/ns/1.0"
 xmlns:msxsl="urn:schemas-microsoft-com:xslt"
 version="1.0"
 extension-element-prefixes="msxsl tei"
 exclude-result-prefixes="h w v w10 sl aml wx o dt st1 msxsl tei">
  
  <!-- Namespace reference:
       aml:  Annotation Mark-up Langauge: generic annotations for all M$ XML.
       h:    HTML, of course.
       o:    Global M$ Office elements.
       w:    WordML: elements for Word documents.
       wx:   WordML Auxiliary. These are hint elements; do not rely upon them!
       v:    VML: M$'s Vector Mark-up Language, because they're too good for SVG.
  -->

<!--msxsl:script language="VisualBasic" implements-prefix="tei">

Public Function makeImageFile(ByVal data As String, imgid As String) As String

Dim b As Byte();
b = System.Convert.FromBase64String(data);
Dim outfile As New System.IO.FileStream(imgid,System.IO.FileMode.Create,System.IO.FileAccess.Write);
outfile.Write(b,0,b.Length);
outfile.Close();
Return imgid;

</msxsl:script-->


  <xsl:output indent="yes"/>

  <xsl:key name="bookmarksByFollowing" match="aml:annotation[@w:type='Word.Bookmark.Start']" use="generate-id((following::w:*|following::wx:*)[1])"/>
  <xsl:key name="bookmarksByName" match="aml:annotation[@w:type='Word.Bookmark.Start']" use="@w:name"/>
  <xsl:key name="getStyle" match="w:style" use="@w:styleId"/>
  <xsl:key name="getListDef" match="w:listDef" use="../w:list[w:ilst/@w:val=current()/@w:listDefId]/@w:ilfo"/>
  <xsl:key name="listStarts" match="w:p[w:pPr/w:listPr][not(w:pPr/w:listPr/w:ilfo/@w:val=preceding-sibling::w:*[1]/w:pPr/w:listPr/w:ilfo/@w:val and w:pPr/w:listPr/w:ilvl/@w:val=preceding-sibling::w:*[1]/w:pPr/w:listPr/w:ilvl/@w:val)]" use="concat(w:pPr/w:listPr/w:ilfo/@w:val,':',w:pPr/w:listPr/w:ilvl/@w:val)"/>


  <!-- xsl:key name="p.bookmarks" match="aml:annotation[@w:type='Word.Bookmark.Start']" use="ancestor::w:p|following-sibling::w:*[1][self::w:p]"/-->

  <!-- ==== User params ==== -->
  <!-- Compound: set to 'true' to preserve the original WordML elements intermixed with the TEI elements. -->
  <xsl:param name="compound" select="'false'"/>

  <!-- Authority text: set to the text to be included as the publishing authority in the TEI Header -->
  <xsl:param name="authorityText"></xsl:param>

  <!-- Exclude bullet item tokens. In the case that $includeItemTokensAsN is 'true', if this is also
       'true' then bullets are *not* written to the n attribute. -->
  <xsl:param name="excludeBulletItemTokens" select="'true'"/>

  <!-- File root: absolute path to the directory into which images are to be written -->
  <xsl:param name="fileRoot">C:\Projects\OUCS</xsl:param>

  <!-- Include item tokens as n. If 'true' (string) the item token in lists (wx:t) is included
       in the generated <item> element as the n attribute. -->
  <xsl:param name="includeItemTokensAsN" select="'true'"/>

  <!-- Publication email: set to the email address for the TEI Header -->
  <xsl:param name="publicationEmail"></xsl:param>

  <!-- Rend Terms: set to the terms the rend vocabulary required. Currently permits CSS or OUCS. -->
  <xsl:param name="rendTerms">CSS</xsl:param>

  <!-- Revision item text: the text to be dropped into the <item> element of the revision description in the TEI Header. -->
  <xsl:param name="revisionItemText"></xsl:param>

  <!-- Run style array: an associative list of style names and element names, structured '|style1,element1|style2,element2|'.
       If a w:r (run) element is matched which has a style corresponding to one of the named styles, the corresponding
       element is generated instead of <hi>. Thus using the above list
          <w:r><w:rPr><w:rStyle w:val='style1'></w:rPr><w:t>Hello world!</w:t></w:r>
       would output
          <element1>Hello world!</element1> -->
  <xsl:param name="runStyleArray"></xsl:param>

  <!-- Sub-section element array: an associative list structured as for $runStyleArray which pairs heading styles and element
       names. In the event of a wx:sub-section element having as its first child a w:p element matching one of the styles,
       the corresponding element is output instead of the <div>. -->
  <xsl:param name="subsectionElementArray"></xsl:param>

  <!-- Sub-section type array: an associative list structured as for $runStyleArray which pairs heading styles and type attribute
       values. In the event of a wx:sub-section element having as its first child a w:p element matching one of the styles,
       the corresponding value is output as the type attribute of the <div> (or other element if provided by 
       $sectionElementArray). -->
  <xsl:param name="subsectionTypeArray"></xsl:param>

  <!-- Source description text: the text to be dropped into the source element in the TEI Header. -->
  <xsl:param name="sourceDescText">Written in Word, converted by wordtotei.xsl</xsl:param>

  <!-- Strip item token of: a list of characters to be removed from item tokens if $includeItemTokensAsN='true'. -->
  <xsl:param name="stripItemTokensOf">().</xsl:param>

  <!-- Tab char: the character with which to replace w:tab. -->
  <xsl:param name="tabChar"> </xsl:param>

  <!-- URL root: the root of the URL to which image links should point. Should correspond to the directory contained in $fileRoot. -->
  <xsl:param name="urlroot">http://zooweb.zoo.ox.ac.uk/</xsl:param>

  <!-- List types. Word defines 59 different types of list. They are referenced in WordML as the w:nfc value in the w:listDef
       element. These 59 parameters define the output type. -->
  <!-- Commonly used lists as their CSS equivalents-->
  <xsl:param name="listTypeArabic">decimal</xsl:param><!-- NFC: 0 -->
  <xsl:param name="listTypeUCRoman">upper-roman</xsl:param><!-- NFC: 1 -->
  <xsl:param name="listTypeLCRoman">lower-roman</xsl:param><!-- NFC: 2 -->
  <xsl:param name="listTypeUCLetter">upper-latin</xsl:param><!-- NFC: 3 -->
  <xsl:param name="listTypeLCLetter">lower-latin</xsl:param><!-- NFC: 4 -->
  <xsl:param name="listTypeBullet">bullet</xsl:param><!-- NFC: 23 -->

  <!-- Other Word list types -->
  <xsl:param name="listTypeOrdinal">Ordinal: 1st, 2nd, 3rd</xsl:param><!-- NFC: 5 -->
  <xsl:param name="listTypeCardtext">Cardinal: One, Two, Three</xsl:param><!-- NFC: 6 -->
  <xsl:param name="listTypeOrdtext">Ordinal Text: First, Second, Third</xsl:param><!-- NFC: 7 -->
  <xsl:param name="listTypeHex">Hexadecimal: 8, 9, A, B, C, D, E, F, 10, 11, 12</xsl:param><!-- NFC: 8 -->
  <xsl:param name="listTypeChiManSty">Chicago Manual of Style</xsl:param><!-- NFC: 9 -->
  <xsl:param name="listTypeDbNum1">Ideograph-digital</xsl:param><!-- NFC: 10 -->
  <xsl:param name="listTypeDbNum2">Japanese counting</xsl:param><!-- NFC: 11 -->
  <xsl:param name="listTypeAiueo">Aiueo</xsl:param><!-- NFC: 12 -->
  <xsl:param name="listTypeIroha">Iroha</xsl:param><!-- NFC: 13 -->
  <xsl:param name="listTypeDbChar">Full-width Arabic: 1, 2, 3, 4</xsl:param><!-- NFC: 14 -->
  <xsl:param name="listTypeSbChar">Half-width Arabic: 1, 2, 3, 4</xsl:param><!-- NFC: 15 -->
  <xsl:param name="listTypeDbNum3">Japanese legal</xsl:param><!-- NFC: 16 -->
  <xsl:param name="listTypeDbNum4">Japanese digital ten thousand</xsl:param><!-- NFC: 17 -->
  <xsl:param name="listTypeCirclenum">Enclosed circles</xsl:param><!-- NFC: 18 -->
  <xsl:param name="listTypeDArabic">Decimal full width2:</xsl:param><!-- NFC: 19 -->
  <xsl:param name="listTypeDAiueo">Aiueo full width</xsl:param><!-- NFC: 20 -->
  <xsl:param name="listTypeDIroha">Iroha full width</xsl:param><!-- NFC: 21 -->
  <xsl:param name="listTypeArabicLZ">Leading zero: 01, 02, ..., 09, 10, 11, ...</xsl:param><!-- NFC: 22 -->
  <xsl:param name="listTypeGanada">Korean Ganada</xsl:param><!-- NFC: 24 -->
  <xsl:param name="listTypeChosung">Korea Chosung</xsl:param><!-- NFC: 25 -->
  <xsl:param name="listTypeGB1">Enclosed full stop</xsl:param><!-- NFC: 26 -->
  <xsl:param name="listTypeGB2">Enclosed parenthesis</xsl:param><!-- NFC: 27 -->
  <xsl:param name="listTypeGB3">Enclosed circle Chinese</xsl:param><!-- NFC: 28 -->
  <xsl:param name="listTypeGB4">Ideograph enclosed circle</xsl:param><!-- NFC: 29 -->
  <xsl:param name="listTypeZodiac1">Ideograph traditional</xsl:param><!-- NFC: 30 -->
  <xsl:param name="listTypeZodiac2">Ideograph Zodiac</xsl:param><!-- NFC: 31 -->
  <xsl:param name="listTypeZodiac3">Ideograph Zodiac traditional</xsl:param><!-- NFC: 32 -->
  <xsl:param name="listTypeTpeDbNum1">Taiwanese counting</xsl:param><!-- NFC: 33 -->
  <xsl:param name="listTypeTpeDbNum2">Ideograph legal traditional</xsl:param><!-- NFC: 34 -->
  <xsl:param name="listTypeTpeDbNum3">Taiwanese counting thousand</xsl:param><!-- NFC: 35 -->
  <xsl:param name="listTypeTpeDbNum4">Taiwanese digital</xsl:param><!-- NFC: 36 -->
  <xsl:param name="listTypeChnDbNum1">Chinese counting</xsl:param><!-- NFC: 37 -->
  <xsl:param name="listTypeChnDbNum2">Chinese legal simplified</xsl:param><!-- NFC: 38 -->
  <xsl:param name="listTypeChnDbNum3">Chinese counting thousand</xsl:param><!-- NFC: 39 -->
  <xsl:param name="listTypeChnDbNum4">Chinese (not implemented)</xsl:param><!-- NFC: 40 -->
  <xsl:param name="listTypeKorDbNum1">Korean digital</xsl:param><!-- NFC: 41 -->
  <xsl:param name="listTypeKorDbNum2">Korean counting</xsl:param><!-- NFC: 42 -->
  <xsl:param name="listTypeKorDbNum3">Korea legal</xsl:param><!-- NFC: 43 -->
  <xsl:param name="listTypeKorDbNum4">Korea digital2</xsl:param><!-- NFC: 44 -->
  <xsl:param name="listTypeHebrew1">Hebrew-1</xsl:param><!-- NFC: 45 -->
  <xsl:param name="listTypeArabic1">Arabic alpha</xsl:param><!-- NFC: 46 -->
  <xsl:param name="listTypeHebrew2">Hebrew-2</xsl:param><!-- NFC: 47 -->
  <xsl:param name="listTypeArabic2">Arabic abjad</xsl:param><!-- NFC: 48 -->
  <xsl:param name="listTypeHindi1">Hindi vowels</xsl:param><!-- NFC: 49 -->
  <xsl:param name="listTypeHindi2">Hindi consonants</xsl:param><!-- NFC: 50 -->
  <xsl:param name="listTypeHindi3">Hindi numbers</xsl:param><!-- NFC: 51 -->
  <xsl:param name="listTypeHindi4">Hindi descriptive (cardinals)</xsl:param><!-- NFC: 52 -->
  <xsl:param name="listTypeThai1">Thai letters</xsl:param><!-- NFC: 53 -->
  <xsl:param name="listTypeThai2">Thai numbers</xsl:param><!-- NFC: 54 -->
  <xsl:param name="listTypeThai3">Thai descriptive (cardinals)</xsl:param><!-- NFC: 55 -->
  <xsl:param name="listTypeViet1">Vietnamese descriptive (cardinals)</xsl:param><!-- NFC: 56 -->
  <xsl:param name="listTypeNumInDash">Page number format: - 1 -, - 2 -, - 3 -, - 4 -</xsl:param><!-- NFC: 57 -->
  <xsl:param name="listTypeLCRus">Lowercase Russian alphabet</xsl:param><!-- NFC: 58 -->
  <xsl:param name="listTypeUCRus">Uppercase Russian alphabet</xsl:param><!-- NFC: 59 -->




  <!-- ==== Non-user variables ==== -->
  <xsl:variable name="whitespace">#x20;#x9;#xD;#xA;</xsl:variable>

  <!-- ==== Matching templates ==== -->

  <!-- == Word Document == -->

  <xsl:template match="w:wordDocument">
    <!-- Select the mode to operate in; mode="compound" copies all Word elements as well as generating TEI ones.
         Note that in both cases the apply-templates is just to w:body (the document body). In the case of non-
         compound documents the w:ignoreSubtree ... w:docPr elements are therefore simply omitted. In the case 
         of compound documents they will be included in header.compound. -->
    <xsl:choose>
      <xsl:when test="$compound='true'">
	<xsl:message terminate="yes">Compound mode not yet ready.</xsl:message>
	<xsl:copy>
	  <xsl:copy-of select="@*"/>
	  <!-- Generic template match in compound mode copies in all Word document description elements. -->
	  <xsl:apply-templates mode="compound"/>
	</xsl:copy>
      </xsl:when>
      <xsl:otherwise>
	<!-- Note in non-compound mode apply-templates is restricted to w:body -->
	<xsl:apply-templates select="w:body"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- ==== Non-compound (null) mode ==== -->

  <xsl:template match="w:body">
    <TEI>
      <xsl:call-template name="header"/>
      <text>
	<body>
	  <xsl:apply-templates/>  
	</body>
      </text>
    </TEI>
  </xsl:template>

  <xsl:template match="w:br|w:cr">
    <lb/>
  </xsl:template>

  <xsl:template match="w:endnote|w:footnote">
    <note>
      <xsl:attribute name="place">
	<xsl:choose>
	  <xsl:when test="self::w:endnote">
	    <xsl:text>end</xsl:text>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:text>foot</xsl:text>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:attribute>
      <xsl:apply-templates/>
    </note>
  </xsl:template>

  <xsl:template match="w:hlink">
    <xsl:choose>
      <xsl:when test="key('bookmarksByName',@w:bookmark)">
	<ref target="ID.{key('bookmarksByName',@w:bookmark)[1]/@aml:id}">
	  <xsl:apply-templates/>
	</ref>
      </xsl:when>
      <xsl:when test="@w:dest=string(.)">
	<ptr target="#{@w:dest}"/>
      </xsl:when>
      <xsl:when test="@w:dest">
	<ref target="@w:dest">
	  <xsl:apply-templates/>
	</ref>
      </xsl:when>
      <xsl:otherwise>
	<xsl:comment>Unresolved hyperlink:</xsl:comment>
	<xsl:apply-templates/>
	<xsl:comment>End unresolved hyperlink</xsl:comment>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="w:noBreakHyphen">
    <xsl:text>&#x2d;;</xsl:text>
  </xsl:template>

  <xsl:template match="w:p[w:pPr/w:listPr]">
    <xsl:variable name="ilfo" select="w:pPr/w:listPr/w:ilfo/@w:val"/>
    <xsl:variable name="ilvl" select="w:pPr/w:listPr/w:ilvl/@w:val"/>
    <xsl:variable name="listType">
      <xsl:call-template name="getListType">
	<xsl:with-param name="ilfo" select="$ilfo"/>
	<xsl:with-param name="ilvl" select="$ilvl"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="listStarts" select="key('listStarts',concat($ilfo,':',$ilvl))"/>
    <xsl:choose>
      <xsl:when test="preceding-sibling::w:*[1][self::w:p]/w:pPr/w:listPr/w:ilfo/@w:val=$ilfo">
	<!-- This is midway through a list: ignore it -->
      </xsl:when>
      <xsl:otherwise>
	<list>
	  <xsl:if test="string-length($listType)&gt;0">
	    <xsl:attribute name="type">
	      <xsl:value-of select="normalize-space($listType)"/>
	    </xsl:attribute>
	  </xsl:if>
	  <xsl:if test="count($listStarts)&gt;1">
	    <!-- This list is split -->
	    <xsl:attribute name="id">
	      <xsl:value-of select="generate-id(.)"/>
	    </xsl:attribute>
	    <xsl:choose>
	      <xsl:when test="count($listStarts[1]|.)=1">
		<!-- This is the first part of the list; output next only -->
		<xsl:attribute name="next">
		  <xsl:value-of select="generate-id($listStarts[2])"/>
		</xsl:attribute>
	      </xsl:when>
	      <xsl:when test="count($listStarts[position()=last()]|.)=1">
		<!-- This is the last part of the list; output prev only -->
		<xsl:attribute name="prev">
		  <xsl:value-of select="generate-id($listStarts[position()=last()-1])"/>
		</xsl:attribute>
	      </xsl:when>
	      <xsl:otherwise>
		<!-- This is midway through the list; output prev and next -->
		<xsl:attribute name="prev">
		  <xsl:for-each select="preceding::w:p[count($listStarts|.)=count($listStarts)][1]">
		    <xsl:value-of select="generate-id(.)"/>
		  </xsl:for-each>
		</xsl:attribute>
		<xsl:attribute name="next">
		  <xsl:for-each select="following::w:p[count($listStarts|.)=count($listStarts)][1]">
		    <xsl:value-of select="generate-id(.)"/>
		  </xsl:for-each>
		</xsl:attribute>
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:if>
	  <xsl:call-template name="writeListItem"/>
	  <xsl:call-template name="iterateListItems">
	    <xsl:with-param name="ilfo" select="$ilfo"/>
	    <xsl:with-param name="ilvl" select="$ilvl"/>
	  </xsl:call-template>
	</list>
      </xsl:otherwise>	
    </xsl:choose>
  </xsl:template>

  <xsl:template name="getListType">
    <xsl:param name="ilfo"/>
    <xsl:param name="ilvl"/>
    <xsl:for-each select="key('getListDef',$ilfo)[1]">
      <xsl:variable name="newIlvl">
	<xsl:choose>
	  <xsl:when test="w:lvl[@w:ilvl=$ilvl]">
	    <xsl:value-of select="$ilvl"/>
	  </xsl:when>
	  <xsl:when test="w:lvl[@w:ilvl=number($ilvl) mod 3]">
	    <xsl:value-of select="number($ilvl) mod 3"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:text>0</xsl:text>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:variable>
      <xsl:for-each select="w:lvl[@w:ilvl=$newIlvl]">
	<xsl:choose>
	  <!-- Word defines lists by NFC numbers. Params cited provide the type for that list -->
	  <xsl:when test="w:nfc/@w:val='0'"><xsl:value-of select="$listTypeArabic"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='1'"><xsl:value-of select="$listTypeUCRoman"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='2'"><xsl:value-of select="$listTypeLCRoman"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='3'"><xsl:value-of select="$listTypeUCLetter"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='4'"><xsl:value-of select="$listTypeLCLetter"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='5'"><xsl:value-of select="$listTypeOrdinal"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='6'"><xsl:value-of select="$listTypeCardtext"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='7'"><xsl:value-of select="$listTypeOrdtext"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='8'"><xsl:value-of select="$listTypeHex"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='9'"><xsl:value-of select="$listTypeChiManSty"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='10'"><xsl:value-of select="$listTypeDbNum1"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='11'"><xsl:value-of select="$listTypeDbNum2"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='12'"><xsl:value-of select="$listTypeAiueo"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='13'"><xsl:value-of select="$listTypeIroha"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='14'"><xsl:value-of select="$listTypeDbChar"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='15'"><xsl:value-of select="$listTypeSbChar"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='16'"><xsl:value-of select="$listTypeDbNum3"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='17'"><xsl:value-of select="$listTypeDbNum4"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='18'"><xsl:value-of select="$listTypeCirclenum"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='19'"><xsl:value-of select="$listTypeDArabic"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='20'"><xsl:value-of select="$listTypeDAiueo"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='21'"><xsl:value-of select="$listTypeDIroha"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='22'"><xsl:value-of select="$listTypeArabicLZ"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='23'"><xsl:value-of select="$listTypeBullet"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='24'"><xsl:value-of select="$listTypeGanada"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='25'"><xsl:value-of select="$listTypeChosung"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='26'"><xsl:value-of select="$listTypeGB1"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='27'"><xsl:value-of select="$listTypeGB2"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='28'"><xsl:value-of select="$listTypeGB3"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='29'"><xsl:value-of select="$listTypeGB4"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='30'"><xsl:value-of select="$listTypeZodiac1"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='31'"><xsl:value-of select="$listTypeZodiac2"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='32'"><xsl:value-of select="$listTypeZodiac3"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='33'"><xsl:value-of select="$listTypeTpeDbNum1"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='34'"><xsl:value-of select="$listTypeTpeDbNum2"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='35'"><xsl:value-of select="$listTypeTpeDbNum3"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='36'"><xsl:value-of select="$listTypeTpeDbNum4"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='37'"><xsl:value-of select="$listTypeChnDbNum1"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='38'"><xsl:value-of select="$listTypeChnDbNum2"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='39'"><xsl:value-of select="$listTypeChnDbNum3"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='40'"><xsl:value-of select="$listTypeChnDbNum4"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='41'"><xsl:value-of select="$listTypeKorDbNum1"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='42'"><xsl:value-of select="$listTypeKorDbNum2"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='43'"><xsl:value-of select="$listTypeKorDbNum3"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='44'"><xsl:value-of select="$listTypeKorDbNum4"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='45'"><xsl:value-of select="$listTypeHebrew1"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='46'"><xsl:value-of select="$listTypeArabic1"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='47'"><xsl:value-of select="$listTypeHebrew2"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='48'"><xsl:value-of select="$listTypeArabic2"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='49'"><xsl:value-of select="$listTypeHindi1"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='50'"><xsl:value-of select="$listTypeHindi2"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='51'"><xsl:value-of select="$listTypeHindi3"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='52'"><xsl:value-of select="$listTypeHindi4"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='53'"><xsl:value-of select="$listTypeThai1"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='54'"><xsl:value-of select="$listTypeThai2"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='55'"><xsl:value-of select="$listTypeThai3"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='56'"><xsl:value-of select="$listTypeViet1"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='57'"><xsl:value-of select="$listTypeNumInDash"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='58'"><xsl:value-of select="$listTypeLCRus"/></xsl:when>
	  <xsl:when test="w:nfc/@w:val='59'"><xsl:value-of select="$listTypeUCRus"/></xsl:when>
	  <xsl:otherwise><xsl:value-of select="$listTypeArabic"/></xsl:otherwise><!-- Word's default is Arabic -->
	</xsl:choose>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="w:p[not(w:pPr/w:listPr)]"> 
    <!-- Call a recursive template that iterates through styles checking if any of them are headings -->
    <xsl:variable name="isHeading">
      <xsl:call-template name="styleCheck">
	<xsl:with-param name="checkText" select="'Heading'"/>
	<xsl:with-param name="styleId" select="w:pPr/w:pStyle/@w:val"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="rendValues">
      <xsl:for-each select="w:pPr">
	<xsl:call-template name="constructBlockRend"/>
      </xsl:for-each>
    </xsl:variable>
    <xsl:choose> 
      <xsl:when test=".=''">
	<xsl:call-template name="addBookmarks">
	  <xsl:with-param name="asID" select="'false'"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="$isHeading='true'">
	<head>
	  <xsl:if test="string-length(normalize-space($rendValues))&gt;0">
	    <xsl:attribute name="rend">
	      <xsl:value-of select="normalize-space($rendValues)"/>
	    </xsl:attribute>
	  </xsl:if>
	  <xsl:call-template name="addBookmarks"/>
	  <xsl:apply-templates/>
	</head>
      </xsl:when>
      <xsl:when test="parent::w:tc">
	<xsl:apply-templates/>
      </xsl:when>
      <xsl:otherwise>
	<p>
	  <xsl:if test="string-length(normalize-space($rendValues))&gt;0">
	    <xsl:attribute name="rend">
	      <xsl:value-of select="normalize-space($rendValues)"/>
	    </xsl:attribute>
	  </xsl:if>
	  <xsl:call-template name="addBookmarks"/>
	  <xsl:apply-templates/>
	</p>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="w:pict">
    <xsl:variable name="pictid" select="concat($fileRoot,substring-after(w:binData/@w:name,'//'))"/>
    <xsl:call-template name="addBookmarks">
      <xsl:with-param name="asID" select="'false'"/>
    </xsl:call-template>
    <figure url="{concat($urlroot,substring-after($pictid,$fileRoot))}"/>
  </xsl:template>

  <xsl:template match="w:r">
    <!-- This handles text runs generically. A look-up list of style-output element pairs ($runStyleArray) is
         used to output an element of a given name given a particular style in the source document; if there is no
	 match for this then the value of constructRunRend is checked, and if this has a value then a <hi> element
	 is output with the corresponding value as the rend attribute. Otherwise, it is treated as a unemphasised
	 text run (i.e. no TEI element). More complex requirements should be handled by specific templates in the
	 importing stylesheet. -->
    <xsl:variable name="runStyle" select="substring-before(substring-after(normalize-space($runStyleArray),concat('|',w:rPr/w:rStyle/@w:val,',')),'|')"/>
    <xsl:variable name="runRend">
      <xsl:for-each select="w:rPr">
	<xsl:call-template name="constructRunRend"/>
      </xsl:for-each>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="string-length($runStyle)&gt;0">
	<xsl:element name="{$runStyle}">
	  <xsl:call-template name="addBookmarks">
	    <xsl:with-param name="asID" select="'false'"/>
	  </xsl:call-template>
	  <xsl:apply-templates/>
	</xsl:element>
      </xsl:when>
      <xsl:when test="string-length($runRend)&gt;0">
	<hi rend="{$runRend}">
	  <xsl:call-template name="addBookmarks">
	    <xsl:with-param name="asID" select="'false'"/>
	  </xsl:call-template>
	  <xsl:apply-templates/>
	</hi>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="addBookmarks">
	  <xsl:with-param name="asID" select="'false'"/>
	</xsl:call-template>
	<xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="w:softHyphen">
    <xsl:call-template name="addBookmarks">
      <xsl:with-param name="asID" select="'false'"/>
    </xsl:call-template>
    <xsl:text>&#xad;</xsl:text>
  </xsl:template>

  <xsl:template match="w:sym">
    <!-- Look, Word represents some characters with a w:sym element that contains a mapping
	 (@w:char) to a Unicode Private User Area code. How useful. We shall strip it. -->
    <xsl:call-template name="addBookmarks">
      <xsl:with-param name="asID" select="'false'"/>
    </xsl:call-template>
    <xsl:comment>Word Symbol element omitted</xsl:comment>
  </xsl:template>

  <xsl:template match="w:tab">
    <xsl:call-template name="addBookmarks">
      <xsl:with-param name="asID" select="'false'"/>
    </xsl:call-template>
    <xsl:copy-of select="tabChar"/>
  </xsl:template>

  <xsl:template match="w:tbl">
    <xsl:variable name="rendValues">
      <xsl:for-each select="w:tblPr">
	<xsl:call-template name="constructBlockRend"/>
      </xsl:for-each>
    </xsl:variable>
    <table cols="{count(w:tblGrid/w:gridCol)}" rows="{count(w:tr)}">
      <xsl:call-template name="addBookmarks"/>
      <xsl:apply-templates/>
    </table>
  </xsl:template>

  <xsl:template match="w:tc">
    <xsl:choose>
      <xsl:when test="w:tcPr/w:hmerge[not(@w:val='restart')] or w:tcPr/w:vmerge[not(@w:val='restart')]">
	<!-- Merged cells have "dummy" blanks identified by vmerge or hmerge. We will throw these away. -->
	<xsl:call-template name="addBookmarks">
      <xsl:with-param name="asID" select="'false'"/>
    </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
	<xsl:variable name="rendValues">
	  <xsl:for-each select="w:tcPr">
	    <xsl:call-template name="constructCellRend"/>
	  </xsl:for-each>
	</xsl:variable>
	<cell>
	  <xsl:if test="string-length($rendValues) &gt; 1">
	    <xsl:attribute name="rend">
	      <xsl:value-of select="$rendValues"/>
	    </xsl:attribute>
	  </xsl:if>
	  <xsl:if test="number(w:tcPr/w:gridSpan/@w:val) &gt; 1">
	    <xsl:attribute name="cols">
	      <xsl:value-of select="w:tcPr/w:gridSpan/@w:val"/>
	    </xsl:attribute>
	  </xsl:if>
	  <xsl:if test="w:tcPr/w:vmerge">
	    <xsl:attribute name="rows">
	      <!-- Word has two methods of horizontal merging: hmerge which provides dummy cells, and gridspan,
		   which does not. To obtain the column number then, we sum the gridspans of all gridspanned cells,
		   and add the number of non-gridspanned cells, and then add one. We then iterate through all
		   following rows incrementing the row value if the same cell has a vmerge in it. -->
	      <xsl:call-template name="countVmerge">
		<xsl:with-param name="colno" select="sum(preceding-sibling::w:tc/w:tcPr/w:gridSpan/@w:val) + count(preceding-sibling::w:tc[not(w:tcPr/w:gridSpan)]) + 1"/>
		<xsl:with-param name="rows" select="1"/>
	      </xsl:call-template>
	    </xsl:attribute>
	  </xsl:if>
	  <xsl:if test="w:tcPr/w:hmerge and not(number(w:tcPr/w:gridSpan/@w:val) &gt; 1)">
	    <!-- Not sure what to do if there is hmerge AND gridspan! -->
	    <xsl:attribute name="cols">
	      <xsl:call-template name="countHmerge">
		<xsl:with-param name="cols" select="1"/>
	      </xsl:call-template>
	    </xsl:attribute>
	  </xsl:if>
	  <xsl:call-template name="addBookmarks"/>
	  <xsl:apply-templates/>
	</cell>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="w:tr">
    <row>
      <xsl:call-template name="addBookmarks"/>
      <xsl:apply-templates/>
    </row>
  </xsl:template>

  <xsl:template match="wx:sect">
    <!-- Not really a section -->
    <xsl:call-template name="addBookmarks">
      <xsl:with-param name="asID" select="'false'"/>
    </xsl:call-template>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="wx:sub-section">
    <xsl:if test="not(.='')">
      <xsl:variable name="headingStyle" select="w:p[1]/w:pPr/w:pStyle/@w:val"/>
      <xsl:variable name="replacementName" select="substring-before(substring-after(normalize-space($subsectionElementArray),concat('|',$headingStyle,',')),'|')"/>  
      <xsl:variable name="typeValue" select="substring-before(substring-after(normalize-space($subsectionTypeArray),concat('|',$headingStyle,',')),'|')"/>
      <xsl:variable name="divGI">
	<xsl:choose>
	  <xsl:when test="string-length($replacementName)&gt;1">
	    <xsl:value-of select="$replacementName"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:text>div</xsl:text>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:variable>
      <xsl:element name="{$divGI}">
	<xsl:if test="string-length($typeValue)&gt;1">
	  <xsl:attribute name="type">
	    <xsl:value-of select="$typeValue"/>
	  </xsl:attribute>
	</xsl:if>
	<xsl:call-template name="addBookmarks"/>
	<xsl:apply-templates/>
      </xsl:element>
    </xsl:if>
  </xsl:template>
      

  <!-- Stripped elements -->
  <xsl:template match="w:delInstrText|w:delText|w:fldData|w:fldChar|w:ftr|w:hdr|w:ignoreSubtree|w:ignoreElements|w:instrText|w:permStart|w:permEnd|w:pgNum|w:proofErr|w:ruby|w:subDoc|w:tblPr|w:tblPrEx|w:tblGrid">
    <xsl:call-template name="addBookmarks">
      <xsl:with-param name="asID" select="'false'"/>
    </xsl:call-template>
  </xsl:template>


  <!-- Ignored, but continue containing elements -->
  <xsl:template match="w:fldSimple|w:t">
    <xsl:call-template name="addBookmarks">
      <xsl:with-param name="asID" select="'false'"/>
    </xsl:call-template>
    <xsl:apply-templates/>
  </xsl:template>

  <!-- Non w: elements -->
  <xsl:template match="aml:*|h:*|w:*|v:*|w10:*|sl:*|aml:*|wx:*|o:*|dt:*|st1:*">
    <xsl:apply-templates/>
  </xsl:template>


  <!-- ==== Compound mode ==== -->

  <xsl:template match="w:body" mode="compound">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <!-- Note in compound mode all TEI elements  must occur within w:body -->
      <TEI.2>
	<xsl:call-template name="header.compound"/>
	<text>
	  <body>
	    <xsl:apply-templates mode="compound"/>
	  </body>
	</text>
      </TEI.2>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="w:br|w:cr" mode="compound">
    <!-- Both empty elements: maybe should have a link? -->
    <lb/>
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="w:body//w:endnote|w:body//w:footnote" mode="compound">
    <!-- Don't match footnotes outside the body (appear in w:docPr to supply formatting info) -->
    <note>
      <xsl:attribute name="place">
	<xsl:choose>
	  <xsl:when test="self::w:endnote">
	    <xsl:text>end</xsl:text>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:text>foot</xsl:text>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:attribute>
      <xsl:copy>
	<xsl:copy-of select="@*"/>
	<xsl:apply-templates mode="compound"/>
      </xsl:copy>
    </note>
  </xsl:template>

  <xsl:template match="w:endnote[not(ancestor::w:body)]|w:footnote[not(ancestor::w:body)]" mode="compound">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates mode="compound"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="w:hlink" mode="compound">
    <xsl:choose>
      <xsl:when test="key('bookmarksByName',@w:bookmark)">
	<ref target="{translate(@w:bookmark,$whitespace,'')}">
	  <xsl:copy>
	    <xsl:copy-of select="@*"/>
	    <xsl:apply-templates mode="compound"/>
	  </xsl:copy>
	</ref>
      </xsl:when>
      <xsl:when test="@w:dest=string(.)">
	<xptr url="{@w:dest}"/>
	<xsl:copy>
	  <xsl:copy-of select="@*"/>
	  <xsl:apply-templates mode="compound"/>
	</xsl:copy>
      </xsl:when>
      <xsl:when test="@w:dest">
	<xref url="@w:dest">
	  <xsl:copy>
	    <xsl:copy-of select="@*"/>
	    <xsl:apply-templates mode="compound"/>
	  </xsl:copy>
	</xref>
      </xsl:when>
      <xsl:otherwise>
	<xsl:comment>Unresolved hyperlink:</xsl:comment>
	<xsl:apply-templates mode="compound"/>
	<xsl:comment>End unresolved hyperlink</xsl:comment>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template match="w:noBreakHyphen" mode="compound">
    <xsl:text>&#x2d;;</xsl:text>
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="w:p[w:pPr/w:listPr]" mode="compound">
    <xsl:variable name="ilfo" select="w:pPr/w:listPr/w:ilfo/@w:val"/>
    <xsl:variable name="ilvl" select="w:pPr/w:listPr/w:ilvl/@w:val"/>
    <xsl:variable name="listType">
      <xsl:call-template name="getListType">
	<xsl:with-param name="ilfo" select="$ilfo"/>
	<xsl:with-param name="ilvl" select="$ilvl"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="listStarts" select="key('listStarts',concat($ilfo,':',$ilvl))"/>
    <xsl:choose>
      <xsl:when test="preceding-sibling::w:*[1][self::w:p]/w:pPr/w:listPr/w:ilfo/@w:val=$ilfo">
	<!-- This is midway through a list: ignore it -->
      </xsl:when>
      <xsl:otherwise>
	<list>
	  <xsl:if test="string-length($listType)&gt;0">
	    <xsl:attribute name="type">
	      <xsl:value-of select="normalize-space($listType)"/>
	    </xsl:attribute>
	  </xsl:if>
	  <xsl:if test="count($listStarts)&gt;1">
	    <!-- This list is split -->
	    <xsl:attribute name="id">
	      <xsl:value-of select="generate-id(.)"/>
	    </xsl:attribute>
	    <xsl:choose>
	      <xsl:when test="count($listStarts[1]|.)=1">
		<!-- This is the first part of the list; output next only -->
		<xsl:attribute name="next">
		  <xsl:value-of select="generate-id($listStarts[2])"/>
		</xsl:attribute>
	      </xsl:when>
	      <xsl:when test="count($listStarts[position()=last()]|.)=1">
		<!-- This is the last part of the list; output prev only -->
		<xsl:attribute name="prev">
		  <xsl:value-of select="generate-id($listStarts[position()=last()-1])"/>
		</xsl:attribute>
	      </xsl:when>
	    </xsl:choose>
	  </xsl:if>
	  <xsl:call-template name="writeListItem.compound"/>
	  <xsl:call-template name="iterateListItems.compound">
	    <xsl:with-param name="ilfo" select="$ilfo"/>
	    <xsl:with-param name="ilvl" select="$ilvl"/>
	  </xsl:call-template>
	</list>
      </xsl:otherwise>	
    </xsl:choose>
  </xsl:template>

  <xsl:template match="w:p[not(w:pPr/w:listPr)]" mode="compound"> 
    <!-- Call a recursive template that iterates through styles checking if any of them are headings -->
    <xsl:variable name="isHeading">
      <xsl:call-template name="styleCheck">
	<xsl:with-param name="checkText" select="'Heading'"/>
	<xsl:with-param name="styleId" select="w:pPr/w:pStyle/@w:val"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="rendValues">
      <xsl:for-each select="w:pPr">
	<xsl:call-template name="constructBlockRend"/>
      </xsl:for-each>
    </xsl:variable>
    <xsl:choose> 
      <xsl:when test=".=''">
	<xsl:copy-of select="."/>
      </xsl:when>
      <xsl:when test="$isHeading='true'">
	<head>
	  <w:p>
	    <xsl:copy-of select="@*"/>
	    <xsl:copy-of select="w:pPr"/>
	    <xsl:apply-templates mode="compound"/>
	  </w:p>
	</head>
	</xsl:when>
	<xsl:when test="parent::w:tc">
	  <w:p>
	    <xsl:copy-of select="@*"/>
	    <xsl:copy-of select="w:pPr"/>
	    <xsl:apply-templates mode="compound"/>
	  </w:p>
	</xsl:when>
	<xsl:otherwise>
	  <w:p>
	    <xsl:copy-of select="@*"/>
	    <xsl:copy-of select="w:pPr"/>
	    <p>
	      <xsl:if test="string-length(normalize-space($rendValues))&gt;0">
		<xsl:attribute name="rend">
		  <xsl:value-of select="normalize-space($rendValues)"/>
		</xsl:attribute>
	      </xsl:if>
	      <xsl:apply-templates mode="compound"/>
	    </p>
	  </w:p>
	</xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="w:pPr" mode="compound"/><!-- Delete in apply-templates as copied in directly -->

  <xsl:template match="w:pict" mode="compound">
    <xsl:variable name="pictid" select="concat($fileRoot,substring-after(w:binData/@w:name,'//'))"/>
    <figure url="{concat($urlroot,substring-after($pictid,$fileRoot))}">
      <xsl:copy-of select="."/>
    </figure>
  </xsl:template>

  <xsl:template match="w:r" mode="compound">
    <!-- This handles text runs generically. A look-up list of style-output element pairs ($runStyleArray) is
         used to output an element of a given name given a particular style in the source document; if there is no
	 match for this then the value of constructRunRend is checked, and if this has a value then a <hi> element
	 is output with the corresponding value as the rend attribute. Otherwise, it is treated as a unemphasised
	 text run (i.e. no TEI element). More complex requirements should be handled by specific templates in the
	 importing stylesheet. -->
    <xsl:variable name="runStyle" select="substring-before(substring-after(normalize-space($runStyleArray),concat('|',w:rPr/w:rStyle/@w:val,',')),'|')"/>
    <xsl:variable name="runRend">
      <xsl:for-each select="w:rPr">
	<xsl:call-template name="constructRunRend"/>
      </xsl:for-each>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="string-length($runStyle)&gt;0">
	<xsl:element name="{$runStyle}">
	  <xsl:copy>
	    <xsl:copy-of select="@*"/>
	    <xsl:copy-of select="w:rPr"/>
	    <xsl:apply-templates mode="compound"/>
	  </xsl:copy>
	</xsl:element>
      </xsl:when>
      <xsl:when test="string-length($runRend)&gt;0">
	<hi rend="{$runRend}">
	  <xsl:copy>
	    <xsl:copy-of select="@*"/>
	    <xsl:copy-of select="w:rPr"/>
	    <xsl:apply-templates mode="compound"/>
	  </xsl:copy>
	</hi>
      </xsl:when>
      <xsl:otherwise>
	<xsl:copy>
	  <xsl:copy-of select="@*"/>
	  <xsl:copy-of select="w:rPr"/>
	  <xsl:apply-templates mode="compound"/>
	</xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="w:rPr" mode="compound"/><!-- Deleted as copied in directly by w:r template -->

  <xsl:template match="w:softHyphen" mode="compound">
    <xsl:text>&#xad;</xsl:text>
    <!-- No copy as U+00AD is a substitute. -->
  </xsl:template>

  <xsl:template match="w:tab" mode="compound">
    <xsl:copy-of select="tabChar"/>
    <!-- No copy as tabChar is a substitute -->
  </xsl:template>

  <!-- Copied elements -->
  <xsl:template match="w:delInstrText|w:delText|w:fldData|w:fldChar|w:fldSimple|w:ftr|w:hdr|w:ignoreSubtree|w:ignoreElements|w:instrText|w:permStart|w:permEnd|w:pgNum|w:proofErr|w:ruby|w:subDoc|w:t" mode="compound">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates mode="compound"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="aml:*|h:*|w:*|v:*|w10:*|sl:*|aml:*|wx:*|o:*|dt:*|st1:*" mode="compound">
    <!-- Copy all unused elements -->
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates mode="compound"/>
    </xsl:copy>
  </xsl:template>


  <!-- ==== Named templates ==== -->
  
  <xsl:template name="addBookmarks">
    <!-- Call this template to add preceding bookmarks as anchors within templates matching <w:p>, <w:tbl>, or <w:r>.
	 Note that if there is only one bookmark, and asID is true the value is added as an ID attribute rather than
	 as a separate anchor element. The use of the node param allows the template to be called to operate on other
         nodes than the current node if supplied. -->
    <xsl:param name="asID" select="'true'"/>
    <xsl:param name="node" select="."/>
    <xsl:variable name="bookmarks" select="key('bookmarksByFollowing',generate-id($node))"/>
    <xsl:choose>
      <xsl:when test="count($bookmarks)=1 and $asID='true'">
	<xsl:attribute name="id">
	  <xsl:value-of select="concat('ID.',$bookmarks/@aml:id)"/>
	</xsl:attribute>
      </xsl:when>
      <xsl:otherwise>
	<xsl:for-each select="$bookmarks">
	  <anchor id="ID.{@aml:id}"/>
	</xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="constructBlockRend">
    <!-- Call this template from w:pPr to construct the value of a rend attribute from the added info within
         w:pPr. Choose rather than when is used to allow extension to different rendTerms options. Note 
	 that at present it does *not* handle negation of styles defined in the corresponding w:pStyle (e.g.
         if the w:pStyle references a style that is bold, but this paragraph has w:pPr/w:b/@val='off' this
	 is not handled). -->
    <xsl:choose>
      <xsl:when test="$rendTerms='CSS' and w:keepNext[not(@w:val='off')]">
	<xsl:text>page-break-after:avoid;</xsl:text>
      </xsl:when>
    </xsl:choose>
    <xsl:choose>
      <xsl:when test="$rendTerms='CSS' and w:keepLines[not(@w:val='off')]">
	<xsl:text>page-break-inside:avoid;</xsl:text>
      </xsl:when>
    </xsl:choose>
    <xsl:choose>
      <xsl:when test="$rendTerms='CSS' and w:pageBreakBefore[not(@w:val='off')]">
	<xsl:text>page-break-before:always;</xsl:text>
      </xsl:when>
    </xsl:choose>
    <!-- Not handled: framePr -->
    <xsl:choose>
      <xsl:when test="$rendTerms='CSS' and w:widows[not(@w:val='off')]">
	<xsl:text>widows:2;orphans:2;</xsl:text>
      </xsl:when>
    </xsl:choose>
    <!-- Not handled: listPr (separate template) -->
    <!-- Not handled: supressLineNumbers, pBdr, shd, tabs, suppressAutoHyphens, kinsoku, wordWrap, overflowPunct,
	 topLinePunct, autoSpaceDE, autoSpaceDN, bidi, adjustRightInd, snapToGrid -->
    <xsl:choose>
      <xsl:when test="$rendTerms='CSS' and number(w:spacing/@w:before)=number(w:spacing/@w:before) and
		      not (w:contextualSpacing[not(@w:val='off')] and
		      preceding::w:p[1]/w:pStyle/@w:val=current()/w:pStyle/@w:val)">
	<xsl:value-of select="concat('padding-top:',number(w:spacing/@w:before) div 20,'pt;')"/>
      </xsl:when>
      <xsl:when test="$rendTerms='CSS' and number(w:spacing/@w:before-lines)=number(w:spacing/@w:before-lines) and
		      not (w:contextualSpacing[not(@w:val='off')] and
		      preceding::w:p[1]/w:pStyle/@w:val=current()/w:pStyle/@w:val)">
	<xsl:value-of select="concat('padding-top:',w:spacing/@w:before-lines,'em;')"/>
      </xsl:when>
    </xsl:choose>
    <xsl:choose>
      <xsl:when test="$rendTerms='CSS' and number(w:spacing/@w:after)=number(w:spacing/@w:after) and
		      not (w:contextualSpacing[not(@w:val='off')] and
		      following::w:p[1]/w:pStyle/@w:val=current()/w:pStyle/@w:val)">
	<xsl:value-of select="concat('padding-bottom:',number(w:spacing/@w:after) div 20,'pt;')"/>
      </xsl:when>
      <xsl:when test="number(w:spacing/@w:after-lines)=number(w:spacing/@w:after-lines) and $rendTerms='CSS' and
		      not (w:contextualSpacing[not(@w:val='off')] and
		      following::w:p[1]/w:pStyle/@w:val=current()/w:pStyle/@w:val)">
	<xsl:value-of select="concat('padding-bottom:',w:spacing/@w:after-lines,'em;')"/>
      </xsl:when>
    </xsl:choose>
    <xsl:choose>
      <!-- Note this does not currently allow for mixing indentation values between chars and points
	   (e.g. <w:ind w:left="200" w:first-line-chars="4"/>).
	   To do this the font width needs to be obtained to the indentation can be calcuated.
	   Note the difference between CSS and Word: Word defines w:left as the left edge of the *first* line,
	   and w:hanging is then *added* to that to obtain the tab point for subsequent lines. CSS, defines the left
	   edge of the subsequent lines at padding-left, and uses a negative text-indent.
      -->
      <xsl:when test="$rendTerms='CSS' and number(w:ind/@w:left)=number(w:ind/@w:left) and
		      number(w:ind/@w:first-line)=number(w:ind/@w:first-line)">
	<!-- Left indent and first-line; as points -->
	<xsl:value-of select="concat('padding-left:',w:ind/@w:left div 20,'pt;text-indent:',
			      w:ind/@w:first-line div 20,'pt;')"/>
      </xsl:when>
      <xsl:when test="$rendTerms='CSS' and number(w:ind/@w:left)=number(w:ind/@w:left) and
		      number(w:ind/@w:hanging)=number(w:ind/@w:hanging)">
	<!-- Left indent and hanging; as points -->
	<xsl:value-of select="concat('padding-left:', (w:ind/@w:left + w:ind/@w:hanging) div 20,
			      'pt; text-indent:-', w:ind/@w:hanging div 20, 'pt;')"/>
      </xsl:when>
      <xsl:when test="$rendTerms='CSS' and number(w:ind/@w:left-chars)=number(w:ind/@w:left-chars) and
		      number(w:ind/@w:first-line-chars)=number(w:ind/@w:first-line-chars)">
	<!-- Left indent and first-line; as ems -->
	<xsl:value-of select="concat('padding-left:',w:ind/@w:left,'em;text-indent:',
			      w:ind/@w:first-line,'em;')"/>
      </xsl:when>
      <xsl:when test="$rendTerms='CSS' and number(w:ind/@w:left-chars)=number(w:ind/@w:left-chars) and
		      number(w:ind/@w:hanging-chars)=number(w:ind/@w:hanging-chars)">
	<!-- Left indent and hanging; as ems -->
	<xsl:value-of select="concat('padding-left:', w:ind/@w:left-chars + w:ind/@w:hanging-chars,
			      'em; text-indent:-', w:ind/@w:hanging-chars, 'em;')"/>
      </xsl:when>
      <xsl:when test="$rendTerms='CSS' and number(w:ind/@w:left)=number(w:ind/@w:left)">
	<!-- Just left indent as points -->
	<xsl:value-of select="concat('padding-left:', w:ind/@w:left div 20,'pt;')"/>
      </xsl:when>
      <xsl:when test="$rendTerms='CSS' and number(w:ind/@w:left-chars)=number(w:ind/@w:left-chars)">
	<!-- Just left indent as chars -->
	<xsl:value-of select="concat('padding-left:', w:ind/@w:left-chars,'em;')"/>
      </xsl:when>
      <xsl:when test="$rendTerms='CSS' and number(w:ind/@w:hanging)=number(w:ind/@w:hanging)">
	<!-- Just hanging as points -->
	<xsl:value-of select="concat('padding-left:', w:ind/@w:hanging div 20,'pt;text-indent:-',
			      w:ind/@w:hanging div 20,'pt;')"/>
      </xsl:when>
      <xsl:when test="$rendTerms='CSS' and number(w:ind/@w:hanging-chars)=number(w:ind/@w:hanging-chars)">
	<!-- Just hanging as chars -->
	<xsl:value-of select="concat('padding-left:', w:ind/@w:hanging-chars,'em;text-indent:-',
			      w:ind/@w:hanging-chars,'em;')"/>
      </xsl:when>
      <xsl:when test="$rendTerms='CSS' and number(w:ind/@w:first-line)=number(w:ind/@w:first-line)">
	<!-- Just first line as points -->
	<xsl:value-of select="concat('text-indent:', w:ind/@w:first-line div 20,'pt;')"/>
      </xsl:when>
      <xsl:when test="$rendTerms='CSS' and number(w:ind/@w:first-line-chars)=number(w:ind/@w:first-line-chars)">
	<!-- Just first line as chars -->
	<xsl:value-of select="concat('text-indent:', w:ind/@w:left-chars,'em;')"/>
      </xsl:when>
    </xsl:choose>
    <xsl:choose>
      <xsl:when test="$rendTerms='CSS' and number(w:ind/@w:right)=number(w:ind/@w:right)">
	<xsl:value-of select="concat('padding-right:', w:ind/@w:right div 20,'pt;')"/>
      </xsl:when>
      <xsl:when test="$rendTerms='CSS' and number(w:ind/@w:right-chars)=number(w:ind/@w:right-chars)">
	<xsl:value-of select="concat('padding-right:', w:ind/@w:right-chars,'em;')"/>
      </xsl:when>
    </xsl:choose>
    <!-- Contextual spacing handled in space-before, space-after -->
    <!-- Not handled: suppressOverlap -->
    <xsl:choose>
      <xsl:when test="$rendTerms='OUCS' and w:jc/@w:val">
	<xsl:value-of select="w:jc/@w:val"/>
      </xsl:when>
      <xsl:when test="$rendTerms='CSS' and w:jc/@w:val='left' or w:jc/@w:val='right' or w:jc/@w:val='center'">
	<xsl:value-of select="concat('text-align:',w:jc/@w:val,';')"/>
      </xsl:when>
      <xsl:when test="$rendTerms='CSS' and w:jc/@w:val='both'">
	<xsl:text>text-align:justify;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
	<xsl:text></xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <!-- Not handled: textDirextion, textAlignment, outlineLvl, divId, cnfStyle -->
    <xsl:for-each select="w:rPr">
      <xsl:call-template name="constructRunRend"/>
    </xsl:for-each>
    <!-- Not handled: sectPr -->
  </xsl:template>

  <xsl:template name="constructRunRend">
    <!-- Call this template from w:pPr to construct the value of a rend attribute from the added info within
         w:pPr. Choose rather than when is used to allow extension to different rendTerms options. -->
    <!-- Not handled: rFonts -->
    <xsl:choose>
      <xsl:when test="w:font/@w:name and $rendTerms='CSS'">
	<xsl:value-of select="concat('font-family:',w:font/@w:name,';')"/>
      </xsl:when>
    </xsl:choose>
    <!-- Not handled: sym -->
    <xsl:choose>
      <xsl:when test="(w:b[not(@w:val='off')] or w:b-cs[not(@w:val='off')]) and $rendTerms='CSS'">
	<xsl:text>font-weight:bold;</xsl:text>
      </xsl:when>
      <xsl:when test="(w:b[not(@w:val='off')] or w:b-cs[not(@w:val='off')]) and $rendTerms='OUCS'">
	<xsl:text>bold</xsl:text>
      </xsl:when>
    </xsl:choose>
    <xsl:choose>
      <xsl:when test="(w:i[not(@w:val='off')] or w:i-cs[not(@w:val='off')]) and $rendTerms='CSS'">
	<xsl:text>font-style:italic;</xsl:text>
      </xsl:when>
      <xsl:when test="(w:i[not(@w:val='off')] or w:i-cs[not(@w:val='off')]) and $rendTerms='OUCS'">
	<xsl:text>italic</xsl:text>
      </xsl:when>
    </xsl:choose>
    <xsl:choose>
      <xsl:when test="w:caps[not(@w:val='off')] and $rendTerms='CSS'">
	<xsl:text>text-transform:uppercase;</xsl:text>
      </xsl:when>
    </xsl:choose>
    <xsl:choose>
      <xsl:when test="w:smallCaps[not(@w:val='off')] and $rendTerms='CSS'">
	<xsl:text>font-variant:small-caps;</xsl:text>
      </xsl:when>
    </xsl:choose>
    <xsl:choose>
      <xsl:when test="w:strike[not(@w:val='off')] and $rendTerms='CSS'">
	<xsl:text>text-decoration:line-through;</xsl:text>
      </xsl:when>
    </xsl:choose>
    <!-- Not handled: dstrike, outline, shadow, emboss, imprint, noProof, snapToGrid, vanish, webHidden -->
    <xsl:choose>
      <xsl:when test="w:color/@w:val[not(.='auto')] and $rendTerms='CSS'">
	<xsl:value-of select="concat('color:#',w:color/@w:val,';')"/>
      </xsl:when>
    </xsl:choose>
    <!-- Not handled: spacing, w, position -->
    <xsl:choose>
      <xsl:when test="number(w:sz/@w:val)=number(w:sz/@w:val) and $rendTerms='CSS'">
	<xsl:value-of select="concat('font-size:',number(w:sz/@w:val) div 2,'pt;')"/>
      </xsl:when>
      <xsl:when test="number(w:sz-cs/@w:val)=number(w:sz-cs/@w:val) and $rendTerms='CSS'">
	<xsl:value-of select="concat('font-size:',number(w:sz-cs/@w:val) div 2,'pt;')"/>
      </xsl:when>
    </xsl:choose>
    <!-- Not handled: highlight -->
    <xsl:choose>
      <xsl:when test="w:u[not(@w:val='off')] and $rendTerms='CSS'">
	<xsl:text>text-decoration:underline;</xsl:text>
      </xsl:when>
      <xsl:when test="w:u[not(@w:val='off')] and $rendTerms='OUCS'">
	<xsl:text>ul</xsl:text>
      </xsl:when>
    </xsl:choose>
    <!-- Not handled: effect, bdr, shd, fitText -->
    <xsl:choose>
      <xsl:when test="w:vertAlign/@w:val='subscript' and $rendTerms='CSS'">
	<xsl:text>vertical-align:sub;</xsl:text>
      </xsl:when>
      <xsl:when test="w:vertAlign/@w:val='subscript' and $rendTerms='OUCS'">
	<xsl:text>sub</xsl:text>
      </xsl:when>
      <xsl:when test="w:vertAlign/@w:val='superscript' and $rendTerms='CSS'">
	<xsl:text>vertical-align:super;</xsl:text>
      </xsl:when>
      <xsl:when test="w:vertAlign/@w:val='superscript' and $rendTerms='OUCS'">
	<xsl:text>super</xsl:text>
      </xsl:when>
      <xsl:when test="w:vertAlign/@w:val='baseline' and $rendTerms='CSS'">
	<xsl:text>vertical-align:baseline;</xsl:text>
      </xsl:when>
    </xsl:choose>
    <!-- Not handled: rtl, cs, em, hyphen, lang, asianLayout, specVanish, annotation -->
  </xsl:template>

  <xsl:template name="constructCellRend">
    <!-- Call this template from w:tc to construct the value of a rend attribute from the added info within
	 w:tcPr. Choose rather than when is used to allow extension to different rendTerms options. Note 
	 that at present it does *not* handle negation of styles defined in the corresponding w:cnfStyle. -->
    <xsl:choose>
      <xsl:when test="$rendTerms='CSS' and w:tcW/@w:type='dxa'">
	<xsl:value-of select="concat('width:',w:tcW/@w:w div 20,'pt;')"/>
      </xsl:when>
      <xsl:when test="$rendTerms='CSS' and w:tcW/@w:type='pct'">
	<xsl:value-of select="concat('width:',w:tcW/@w:w,'%;')"/>
      </xsl:when>
    </xsl:choose>
    <!-- gridSpan, hmerge, vmerge handled in cell creation template -->
    <!-- Not handled: tcBorders, shd, noWrap, tcMar, textFlow, tcFitText -->
    <xsl:choose>
      <xsl:when test="$rendTerms='CSS' and (w:vAlign/@w:val='top' or w:vAlign/@w:val='bottom')">
	<xsl:value-of select="concat('vertical-align:',w:vAlign/@w:val,';')"/>
      </xsl:when>
      <xsl:when test="$rendTerms='CSS' and (w:vAlign/@w:val='center')">
	<xsl:text>vertical-align:middle;</xsl:text>
      </xsl:when>
      <!-- How handle "both"? -->
    </xsl:choose>
    <!-- Word does not have a view of cell horizontal alignment; so we just grab it from the first
	 contained paragraph. -->
    <xsl:choose>
      <xsl:when test="$rendTerms='CSS' and (ancestor::w:tc[1]/w:p[1]/w:pPr/w:jc/@w:val='left' or 
		      ancestor::w:tc[1]/w:p[1]/w:pPr/w:jc/@w:val='center' or
		      ancestor::w:tc[1]/w:p[1]/w:pPr/w:jc/@w:val='right')">
	<xsl:value-of select="concat('text-align:',ancestor::w:tc[1]/w:p[1]/w:pPr/w:jc/@w:val,';')"/>
      </xsl:when>
      <xsl:when test="$rendTerms='CSS' and ancestor::w:tc[1]/w:p[1]/w:pPr/w:jc/@w:val='both'">
	<xsl:text>text-align:justify;</xsl:text>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="countHmerge">
    <xsl:param name="cols"/>
    <xsl:choose>
      <xsl:when test="following-sibling::w:tc[1]/w:tcPr/w:hmerge and following-sibling::w:tc[1]/w:tcPr/w:gridSpan">
	<xsl:for-each select="following-sibling::w:tc[1]">
	  <xsl:call-template name="countHmerge">
	    <xsl:with-param name="cols" select="$cols + w:tcPr/w:gridSpan/@w:val"/>
	  </xsl:call-template>
	</xsl:for-each>
      </xsl:when>
      <xsl:when test="following-sibling::w:tc[1]/w:tcPr/w:hmerge">
	<xsl:for-each select="following-sibling::w:tc[1]">
	  <xsl:call-template name="countHmerge">
	    <xsl:with-param name="cols" select="$cols + 1"/>
	  </xsl:call-template>
	</xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="$cols"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="countVmerge">
    <xsl:param name="colno"/>
    <xsl:param name="rows"/>
    <xsl:choose>
      <xsl:when test="ancestor::w:tr[1]/following-sibling::w:tr[1]/w:tc[sum(preceding-sibling::w:tc/w:tcPr/w:gridSpan/@w:val) + count(preceding-sibling::w:tc[not(w:tcPr/w:gridSpan)]) + 1 = $colno]/w:tcPr/w:vmerge[not(@w:val='restart')]">
	<xsl:for-each select="ancestor::w:tr[1]/following-sibling::w:tr[1]/w:tc[sum(preceding-sibling::w:tc/w:tcPr/w:gridSpan/@w:val) + count(preceding-sibling::w:tc[not(w:tcPr/w:gridSpan)]) + 1 = $colno]">
	  <xsl:call-template name="countVmerge">
	    <xsl:with-param name="colno" select="$colno"/>
	    <xsl:with-param name="rows" select="$rows+1"/>
	  </xsl:call-template>
	</xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="$rows"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="findListLevelContinuation">
    <!-- This template is called *after* a subsidiary list, and searches through following siblings to
	 find a potential continuation of the original list. As it is called from the last known item
	 of the original list, it must iterate doing nothing through items of the same list fo but of 
	 a higher level. -->
    <xsl:param name="ilfo"/>
    <xsl:param name="ilvl"/>
    <!-- If this for-each fails it's the end of the entire list group, so do nuffink -->
    <xsl:for-each select="following-sibling::w:*[1][self::w:p/w:pPr/w:listPr]">
      <xsl:choose>
	<xsl:when test="w:pPr/w:listPr/w:ilfo/@w:val=$ilfo and w:pPr/w:listPr/w:ilvl/@w:val=$ilvl">
	  <!-- We have found the next item in the original list, so start again with an item and
	       the list walker. -->
	  <xsl:call-template name="writeListItem"/>
	  <xsl:call-template name="iterateListItems">
	    <xsl:with-param name="ilfo" select="$ilfo"/>
	    <xsl:with-param name="ilvl" select="$ilvl"/>
	  </xsl:call-template>
	</xsl:when>
	<xsl:when test="w:pPr/w:listPr/w:ilfo/@w:val=$ilfo and number(w:pPr/w:listPr/w:ilvl/@w:val) &gt; number($ilvl)">
	  <!-- Same list, higher level. Iterate again. -->
	  <xsl:call-template name="findListLevelContinuation">
	    <xsl:with-param name="ilfo" select="$ilfo"/>
	    <xsl:with-param name="ilvl" select="$ilvl"/>
	  </xsl:call-template>
	</xsl:when>
	<xsl:when test="w:pPr/w:listPr/w:ilfo/@w:val=$ilfo and number(w:pPr/w:listPr/w:ilvl/@w:val) &lt; number($ilvl)">
	  <!-- Same list, lower level. This has happened because we've had a, a.a, a.a.a, a. . Terminate anyway -->
	</xsl:when>
	<xsl:otherwise>
	  <!-- Something wrong -->
	</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="findListLevelContinuation.compound">
    <!-- This template is called *after* a subsidiary list, and searches through following siblings to
	 find a potential continuation of the original list. As it is called from the last known item
	 of the original list, it must iterate doing nothing through items of the same list fo but of 
	 a higher level. -->
    <xsl:param name="ilfo"/>
    <xsl:param name="ilvl"/>
    <!-- If this for-each fails it's the end of the entire list group, so do nuffink -->
    <xsl:for-each select="following-sibling::w:*[1][self::w:p/w:pPr/w:listPr]">
      <xsl:choose>
	<xsl:when test="w:pPr/w:listPr/w:ilfo/@w:val=$ilfo and w:pPr/w:listPr/w:ilvl/@w:val=$ilvl">
	  <!-- We have found the next item in the original list, so start again with an item and
	       the list walker. -->
	  <xsl:call-template name="writeListItem.compound"/>
	  <xsl:call-template name="iterateListItems.compound">
	    <xsl:with-param name="ilfo" select="$ilfo"/>
	    <xsl:with-param name="ilvl" select="$ilvl"/>
	  </xsl:call-template>
	</xsl:when>
	<xsl:when test="w:pPr/w:listPr/w:ilfo/@w:val=$ilfo and number(w:pPr/w:listPr/w:ilvl/@w:val) &gt; number($ilvl)">
	  <!-- Same list, higher level. Iterate again. -->
	  <xsl:call-template name="findListLevelContinuation.compound">
	    <xsl:with-param name="ilfo" select="$ilfo"/>
	    <xsl:with-param name="ilvl" select="$ilvl"/>
	  </xsl:call-template>
	</xsl:when>
	<xsl:when test="w:pPr/w:listPr/w:ilfo/@w:val=$ilfo and number(w:pPr/w:listPr/w:ilvl/@w:val) &lt; number($ilvl)">
	  <!-- Same list, lower level. This has happened because we've had a, a.a, a.a.a, a. . Terminate anyway -->
	</xsl:when>
	<xsl:otherwise>
	  <!-- Something wrong -->
	</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="getAuthor">
    <xsl:apply-templates select="/w:wordDocument/o:DocumentProperties/o:Author"/>
  </xsl:template>

  <xsl:template name="getCreatedDate">
    <xsl:apply-templates select="/w:wordDocument/o:DocumentProperties/o:Created"/>
  </xsl:template>

  <xsl:template name="getLastAuthor">
    <xsl:apply-templates select="/w:wordDocument/o:DocumentProperties/o:LastAuthor"/>
  </xsl:template>

  <xsl:template name="getLastDate">
    <xsl:apply-templates select="/w:wordDocument/o:DocumentProperties/o:LastSaved"/>
  </xsl:template>

  <xsl:template name="getTitle">
    <xsl:apply-templates select="/w:wordDocument/o:DocumentProperties/o:Title"/>
  </xsl:template>

  <xsl:template name="header">
    <!-- This template outputs the teiHeader in non-compound mode -->
    <teiHeader>
      <fileDesc>
        <titleStmt>
	  <title>
	    <xsl:call-template name="getTitle"/>
          </title>
          <author>
            <xsl:call-template name="getAuthor"/>
          </author>
        </titleStmt>
        <editionStmt>
          <edition>
            <date>
	      <xsl:call-template name="getCreatedDate"/>
            </date>
	  </edition>
	</editionStmt>
        <publicationStmt>
          <authority><xsl:value-of select="$authorityText"/></authority>
          <address>
            <email><xsl:value-of select="$publicationEmail"/></email>
          </address>
        </publicationStmt>
        <sourceDesc>
          <p><xsl:value-of select="$sourceDescText"/></p>
        </sourceDesc>
      </fileDesc>
      <revisionDesc>
        <change>
	  <date>            
	    <xsl:call-template name="getLastDate"/>
	  </date>
	  <respStmt>
	    <name> 
	      <xsl:call-template name="getLastAuthor"/>
	    </name>
	  </respStmt>
	  <item><xsl:value-of select="$revisionItemText"/></item>
	</change>
      </revisionDesc>
    </teiHeader>
  </xsl:template>

  <xsl:template name="header.compound">
    <!-- This template outputs the header in compound mode -->
    <!-- We must place all text in w:p/w:r/w:t or else Word will strip it. -->
    <teiHeader>
      <fileDesc>
        <titleStmt>
	  <w:p>
	    <title>
	      <w:r><w:t><xsl:call-template name="getTitle"/></w:t></w:r>
	    </title>
	  </w:p>
	  <w:p>
	    <author>
	      <w:r><w:t><xsl:call-template name="getAuthor"/></w:t></w:r>
	    </author>
	  </w:p>
        </titleStmt>
        <editionStmt>
	  <w:p>
	    <edition>
	      <date>
		<w:r><w:t><xsl:call-template name="getCreatedDate"/></w:t></w:r>
	      </date>
	    </edition>
	  </w:p>
	</editionStmt>
	<publicationStmt>
	  <w:p>
	    <authority><w:r><w:t><xsl:value-of select="$authorityText"/></w:t></w:r></authority>
	  </w:p>
	  <w:p>
	    <address>
	      <email><w:r><w:t><xsl:value-of select="$publicationEmail"/></w:t></w:r></email>
	    </address>
	  </w:p>
        </publicationStmt>
        <sourceDesc>
	  <w:p>
	    <p><w:r><w:t><xsl:value-of select="$sourceDescText"/></w:t></w:r></p>
	  </w:p>
	</sourceDesc>
      </fileDesc>
      <revisionDesc>
        <change>
	  <w:p>
	    <date>            
	      <w:r><w:t><xsl:call-template name="getLastDate"/></w:t></w:r>
	    </date>
	  </w:p>
	  <w:p>
	    <respStmt>
	      <name> 
		<w:r><w:t><xsl:call-template name="getLastAuthor"/></w:t></w:r>
	      </name>
	    </respStmt>
	  </w:p>
	  <w:p>
	    <item>
	      <w:r><w:t><xsl:value-of select="$revisionItemText"/></w:t></w:r>
	    </item>
	  </w:p>
	</change>
      </revisionDesc>
    </teiHeader>
  </xsl:template>

  <xsl:template name="iterateListItems">
    <xsl:param name="ilfo"/>
    <xsl:param name="ilvl"/>
    <xsl:param name="pos" select="count(preceding::w:p[not(w:pPr/w:listPr)]|preceding::w:tbl)"/>
    <xsl:for-each select="following-sibling::w:*[1][self::w:p][w:pPr/w:listPr/w:ilfo/@w:val=$ilfo]">
      <xsl:choose>
	<xsl:when test="w:pPr/w:listPr/w:ilvl/@w:val=$ilvl">
	  <!-- Same level just output an item -->
	  <xsl:call-template name="writeListItem"/>
	  <xsl:call-template name="iterateListItems">
	    <xsl:with-param name="ilfo" select="$ilfo"/>
	    <xsl:with-param name="ilvl" select="$ilvl"/>
	  </xsl:call-template>
	</xsl:when>
	<xsl:when test="number(w:pPr/w:listPr/w:ilvl/@w:val) &gt; number($ilvl)">
	  <!-- Subsidiary list -->
	  <xsl:variable name="newIlvl" select="w:pPr/w:listPr/w:ilvl/@w:val"/>
	  <xsl:variable name="listType">
	    <xsl:call-template name="getListType">
	      <xsl:with-param name="ilfo" select="$ilfo"/>
	      <xsl:with-param name="ilvl" select="$newIlvl"/>
	    </xsl:call-template>
	  </xsl:variable>
	  <xsl:variable name="listStarts" select="key('listStarts',concat($ilfo,':',$newIlvl))"/>
	  <item>
	    <list>
	      <xsl:if test="string-length($listType)&gt;0">
		<xsl:attribute name="type">
		  <xsl:value-of select="normalize-space($listType)"/>
		</xsl:attribute>
	      </xsl:if>
	      <xsl:if test="count($listStarts)&gt;1">
		<!-- This list is split -->
		<xsl:attribute name="id">
		  <xsl:value-of select="generate-id(.)"/>
		</xsl:attribute>
		<xsl:choose>
		  <xsl:when test="count($listStarts[1]|.)=1">
		    <!-- This is the first part of the list; output next only -->
		    <xsl:attribute name="next">
		      <xsl:value-of select="generate-id($listStarts[2])"/>
		    </xsl:attribute>
		  </xsl:when>
		  <xsl:when test="count($listStarts[position()=last()]|.)=1">
		    <!-- This is the last part of the list; output prev only -->
		    <xsl:attribute name="prev">
		      <xsl:value-of select="generate-id($listStarts[position()=last()-1])"/>
		    </xsl:attribute>
		  </xsl:when>
		</xsl:choose>
	      </xsl:if>
	      <xsl:call-template name="writeListItem"/>
	      <xsl:call-template name="iterateListItems">
		<xsl:with-param name="ilfo" select="$ilfo"/>
		<xsl:with-param name="ilvl" select="$newIlvl"/>
	      </xsl:call-template>
	    </list>
	  </item>
	  <!-- Now check to see if this level is continued after the sublist -->
	  <xsl:call-template name="findListLevelContinuation">
	    <xsl:with-param name="ilfo" select="$ilfo"/>
	    <xsl:with-param name="ilvl" select="$ilvl"/>
	  </xsl:call-template>
	</xsl:when>
	<xsl:otherwise>
	  <!-- Do nothing -->
	</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="iterateListItems.compound">
    <xsl:param name="ilfo"/>
    <xsl:param name="ilvl"/>
    <xsl:param name="pos" select="count(preceding::w:p[not(w:pPr/w:listPr)]|preceding::w:tbl)"/>
    <xsl:for-each select="following-sibling::w:*[1][self::w:p][w:pPr/w:listPr/w:ilfo/@w:val=$ilfo]">
      <xsl:choose>
	<xsl:when test="w:pPr/w:listPr/w:ilvl/@w:val=$ilvl">
	  <!-- Same level just output an item -->
	  <xsl:call-template name="writeListItem.compound"/>
	  <xsl:call-template name="iterateListItems.compound">
	    <xsl:with-param name="ilfo" select="$ilfo"/>
	    <xsl:with-param name="ilvl" select="$ilvl"/>
	  </xsl:call-template>
	</xsl:when>
	<xsl:when test="number(w:pPr/w:listPr/w:ilvl/@w:val) &gt; number($ilvl)">
	  <!-- Subsidiary list -->
	  <xsl:variable name="newIlvl" select="w:pPr/w:listPr/w:ilvl/@w:val"/>
	  <xsl:variable name="listType">
	    <xsl:call-template name="getListType">
	      <xsl:with-param name="ilfo" select="$ilfo"/>
	      <xsl:with-param name="ilvl" select="$newIlvl"/>
	    </xsl:call-template>
	  </xsl:variable>
	  <xsl:variable name="listStarts" select="key('listStarts',concat($ilfo,':',$newIlvl))"/>
	  <item>
	    <list>
	      <xsl:if test="string-length($listType)&gt;0">
		<xsl:attribute name="type">
		  <xsl:value-of select="normalize-space($listType)"/>
		</xsl:attribute>
	      </xsl:if>
	      <xsl:if test="count($listStarts)&gt;1">
		<!-- This list is split -->
		<xsl:attribute name="id">
		  <xsl:value-of select="generate-id(.)"/>
		</xsl:attribute>
		<xsl:choose>
		  <xsl:when test="count($listStarts[1]|.)=1">
		    <!-- This is the first part of the list; output next only -->
		    <xsl:attribute name="next">
		      <xsl:value-of select="generate-id($listStarts[2])"/>
		    </xsl:attribute>
		  </xsl:when>
		  <xsl:when test="count($listStarts[position()=last()]|.)=1">
		    <!-- This is the last part of the list; output prev only -->
		    <xsl:attribute name="prev">
		      <xsl:value-of select="generate-id($listStarts[position()=last()-1])"/>
		    </xsl:attribute>
		  </xsl:when>
		</xsl:choose>
	      </xsl:if>
	      <xsl:call-template name="writeListItem.compound"/>
	      <xsl:call-template name="iterateListItems.compound">
		<xsl:with-param name="ilfo" select="$ilfo"/>
		<xsl:with-param name="ilvl" select="$newIlvl"/>
	      </xsl:call-template>
	    </list>
	  </item>
	  <!-- Now check to see if this level is continued after the sublist -->
	  <xsl:call-template name="findListLevelContinuation.compound">
	    <xsl:with-param name="ilfo" select="$ilfo"/>
	    <xsl:with-param name="ilvl" select="$ilvl"/>
	  </xsl:call-template>
	</xsl:when>
	<xsl:otherwise>
	  <!-- Do nothing -->
	</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="styleCheck">
    <!-- This template recurses through the style definitions starting from the given ID and 
	 returns true if any of them contain the checkText string. -->
    <xsl:param name="checkText"/>
    <xsl:param name="styleId"/>
    <xsl:choose>
      <xsl:when test="contains($styleId,$checkText)">
	<xsl:text>true</xsl:text>
      </xsl:when>
      <xsl:when test="key('getStyle',$styleId)/w:basedOn">
	<xsl:for-each select="key('getStyle',$styleId)/w:basedOn">
	  <xsl:call-template name="styleCheck">
	    <xsl:with-param name="checkText" select="$checkText"/>
	    <xsl:with-param name="styleId" select="@w:val"/>
	  </xsl:call-template>
	</xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
	<xsl:text>false</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="writeListItem">
    <xsl:variable name="rendValues">
      <xsl:for-each select="w:pPr">
	<xsl:call-template name="constructBlockRend"/>
      </xsl:for-each>
    </xsl:variable>
    <xsl:variable name="ilfo" select="w:pPr/w:listPr/w:ilfo/@w:val"/>
    <xsl:variable name="ilvl" select="w:pPr/w:listPr/w:ilvl/@w:val"/>
    <item>
      <xsl:if test="$includeItemTokensAsN">
	<xsl:attribute name="n">
	  <xsl:value-of select="translate(w:pPr/w:listPr/wx:t/@wx:val,$stripItemTokensOf,'')"/>
	</xsl:attribute>
      </xsl:if>
      <xsl:if test="string-length(normalize-space($rendValues))&gt;0">
	<xsl:attribute name="rend">
	  <xsl:value-of select="normalize-space($rendValues)"/>
	</xsl:attribute>
      </xsl:if>
      <xsl:apply-templates/>
    </item>
  </xsl:template>

  <xsl:template name="writeListItem.compound">
    <xsl:variable name="rendValues">
      <xsl:for-each select="w:pPr">
	<xsl:call-template name="constructBlockRend"/>
      </xsl:for-each>
    </xsl:variable>
    <xsl:variable name="ilfo" select="w:pPr/w:listPr/w:ilfo/@w:val"/>
    <xsl:variable name="ilvl" select="w:pPr/w:listPr/w:ilvl/@w:val"/>
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <item>
	<xsl:if test="$includeItemTokensAsN">
	  <xsl:attribute name="n">
	    <xsl:value-of select="translate(w:pPr/w:listPr/wx:t/@wx:val,$stripItemTokensOf,'')"/>
	  </xsl:attribute>
	</xsl:if>
	<xsl:if test="string-length(normalize-space($rendValues))&gt;0">
	  <xsl:attribute name="rend">
	    <xsl:value-of select="normalize-space($rendValues)"/>
	  </xsl:attribute>
	</xsl:if>
	<xsl:apply-templates mode="compound"/>
      </item>
    </xsl:copy>
  </xsl:template>

</xsl:stylesheet>
