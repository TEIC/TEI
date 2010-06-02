<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:cals="http://www.oasis-open.org/specs/tm9901"
		xmlns:iso="http://www.iso.org/ns/1.0"
		xmlns:tei="http://www.tei-c.org/ns/1.0"
		xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		exclude-result-prefixes="tei iso cals tbx"
		version="2.0">
  <!-- $Id$ -->
  
  <xsl:variable name="processor">
    <xsl:value-of select="system-property('xsl:vendor')"/>
  </xsl:variable>
  
  <xsl:key name="ISOMETA" match="*[@iso:meta]" use="@iso:meta"/>
  <xsl:key name="ALLMETA" match="*[@iso:meta]" use="1"/>
  
  <xsl:param name="doclang">en</xsl:param>
  <xsl:param name="showTBXMarkup">false</xsl:param>   

  <xsl:template name="whatsTheDate">
    <xsl:value-of select="format-dateTime(current-dateTime(),'[Y]-[M02]-[D02]T[H02]:[M02]:[s02]Z')"/>
  </xsl:template>
  
  
  <xsl:template name="getiso_meta">
    <xsl:param name="meta"/>
    <xsl:value-of select="key('ISOMETA',$meta)"/>
  </xsl:template>
  
  <!--
      <xsl:choose>
      <xsl:when test="string-length(.)&gt;0">
      <xsl:text>(</xsl:text>
      <xsl:value-of select="."/>
      <xsl:text>)</xsl:text>
      <xsl:choose>
      <xsl:when test=".='20'">Preparation</xsl:when>
      <xsl:when test=".='30'">Committee</xsl:when>
      <xsl:when test=".='40'">Enquiry</xsl:when>
      <xsl:when test=".='50'">Approval</xsl:when>
      <xsl:when test=".='60'">Publication</xsl:when>
      </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
      [unspecified]
      </xsl:otherwise>
      </xsl:choose>
  -->
  
  
  <xsl:template name="getiso_year">
    <xsl:value-of select="substring(key('ISOMETA','docdate'),1,4)"/>
  </xsl:template>
  <xsl:template name="generateTitle">
    <xsl:choose>
      <xsl:when test="string-length(key('ISOMETA','fullTitle'))&gt;0">
	<xsl:value-of select="key('ISOMETA','fullTitle')"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="key('ISOMETA','introductoryTitle')"/>
	<xsl:text> — </xsl:text>
	<xsl:value-of select="key('ISOMETA','mainTitle')"/>
	<xsl:if test="key('ISOMETA','complementaryTitle')">
	  <xsl:text> — </xsl:text>
	  <xsl:value-of select="key('ISOMETA','complementaryTitle')"/>
	</xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template name="makeHTMLHeading">
    <xsl:param name="text"/>
    <xsl:param name="class">title</xsl:param>
    <xsl:param name="level">1</xsl:param>
    <xsl:if test="$class = 'maintitle'">
      <div class="healthwarning">This extract from an ISO document has been created for test purposes only. The design and layout are subject to change by ISO.</div>
     <xsl:element name="h{$level}">
       <xsl:attribute name="class">
         <xsl:value-of select="$class"/>
       </xsl:attribute>
       <xsl:value-of select="substring-before(key('ISOMETA','docReference'),'(')"/><br></br><xsl:value-of select="key('ISOMETA','fullTitle')"/>
     </xsl:element>
    </xsl:if>
  </xsl:template>
  
  
  <xsl:template name="getiso_copyright">
    <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:availability[1]"
			 mode="titlepage">
      <xsl:with-param name="style">zzCopyright</xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>
  
  <xsl:template name="getiso_coverWarning">
    <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:availability[2]"
			 mode="titlepage">
      <xsl:with-param name="style">coverwarning</xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>
  
  <xsl:template name="getiso_header">
    <xsl:value-of select="key('ISOMETA','secretariat')"/>
    <xsl:variable name="stage">
      <xsl:value-of select="key('ISOMETA','docStage')"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$stage='40'">/DIS </xsl:when>
      <xsl:when test="$stage='50'">/FDIS </xsl:when>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template name="docID">
    <xsl:variable name="doclang">
      <xsl:value-of select="ancestor-or-self::tei:TEI/@xml:lang"/>
    </xsl:variable>
    <xsl:variable name="stage">
      <xsl:value-of select="key('ISOMETA','docStage')"/>
    </xsl:variable>
    
    <xsl:value-of select="key('ISOMETA','docNumber')"/>
    <xsl:text>-</xsl:text>
    <xsl:value-of select="key('ISOMETA','docPartNumber')"/>
    
    <xsl:choose>
      <xsl:when test="$stage='20'"/>
      <xsl:when test="$stage='30'"/>
      <xsl:when test="$stage='40'"/>
      <xsl:otherwise>
	<xsl:text>:</xsl:text>
	<xsl:call-template name="getiso_year"/>
	<xsl:choose>
	  <xsl:when test="starts-with($doclang,'en')">(E)</xsl:when>
	  <xsl:when test="starts-with($doclang,'fr')">(F)</xsl:when>
	</xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="getiso_authority">
    <xsl:value-of select="key('ISOMETA','secretariat')"/>
  </xsl:template>
  
  <xsl:template name="getiso_documentNumber">
    <xsl:value-of select="key('ISOMETA','referenceNumber')"/>
  </xsl:template>
  
  <xsl:template name="getiso_partNumber">
    <xsl:value-of select="key('ISOMETA','partNumber')"/>
  </xsl:template>
  
  <xsl:template name="Identify">
    <xsl:for-each select="ancestor::cals:entry">
      <xsl:text> </xsl:text>
      <xsl:call-template name="Identify"/>
      <xsl:text> </xsl:text>
    </xsl:for-each>
    <xsl:choose>
      <xsl:when test="self::tei:p">
	<xsl:call-template name="LabelChange">
	  <xsl:with-param name="What">Paragraph</xsl:with-param>
	  <xsl:with-param
	      name="N"><xsl:number/></xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="self::tei:item">
	<xsl:call-template name="LabelChange">
	  <xsl:with-param name="What">List item</xsl:with-param>
	  <xsl:with-param name="N"><xsl:number/></xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="self::tei:head">
	<xsl:choose>
	  <xsl:when
	      test="count(ancestor::tei:div)=1">Clause title</xsl:when>
	  <xsl:otherwise>Clause subtitle</xsl:otherwise>
	</xsl:choose>
      </xsl:when>
      <xsl:when test="self::cals:entry">
	<xsl:for-each select="ancestor::cals:table">
	  <xsl:text>Table </xsl:text>
	  <xsl:number level="any"/>
	  <xsl:text>, </xsl:text>
	</xsl:for-each>
	<xsl:call-template name="LabelChange">
	  <xsl:with-param name="What">row</xsl:with-param>
	  <xsl:with-param name="N">
	    <xsl:for-each select="parent::cals:row">
	      <xsl:number/>
	  </xsl:for-each></xsl:with-param>
	</xsl:call-template>
	<xsl:text>, </xsl:text>
	<xsl:call-template name="LabelChange">
	  <xsl:with-param name="What">cell</xsl:with-param>
	  <xsl:with-param name="N"><xsl:number/></xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="self::tei:term">
	<xsl:call-template name="LabelChange">
	  <xsl:with-param name="What">Terminology entry</xsl:with-param>
	  <xsl:with-param
	      name="N"><xsl:number/></xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="self::tbx:term">
	<xsl:call-template name="LabelChange">
	  <xsl:with-param name="What">Term</xsl:with-param>
	  <xsl:with-param name="N">
	    <xsl:value-of select="substring-after(@id,'user_')"/>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="self::tei:bibl">
	<xsl:call-template name="LabelChange">
	  <xsl:with-param name="What">Bibliographical entry</xsl:with-param>
	  <xsl:with-param name="N"><xsl:number/></xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="self::tei:math">
	<xsl:call-template name="LabelChange">
	  <xsl:with-param name="What">math</xsl:with-param>
	  <xsl:with-param name="N"><xsl:number/></xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="LabelChange">
	  <xsl:with-param name="What">
	    <xsl:value-of select="local-name()"/>
	  </xsl:with-param>
	  <xsl:with-param name="N"><xsl:number/></xsl:with-param>
	</xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="LabelChange">
    <xsl:param name="What"/>
    <xsl:param name="N"/>
    <xsl:choose>
      <xsl:when test="not(number($N))">
	<xsl:value-of select="$What"/>
	<xsl:text> </xsl:text>
	<xsl:value-of select="$N"/>
      </xsl:when>
      <xsl:when test="$N=1">     
	<xsl:text>1st </xsl:text>
	<xsl:value-of select="$What"/>
      </xsl:when>
      <xsl:when test="$N=2">     
	<xsl:text>2nd </xsl:text>
	<xsl:value-of select="$What"/>
      </xsl:when>
      <xsl:when test="$N=3">     
	<xsl:text>3rd </xsl:text>
	<xsl:value-of select="$What"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="$N"/>
	<xsl:text>th </xsl:text>
	<xsl:value-of select="$What"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template name="simpleRun">
    <xsl:param name="prefix"/>
    <xsl:param name="text"/>
    <xsl:param name="italic"/>
    <xsl:value-of select="$prefix"/>
    <xsl:value-of select="$text"/>
  </xsl:template>
  
  <!-- all the special bibl components -->
  <xsl:template
      match="tei:listBibl/tei:bibl/tei:publisher">
    <xsl:variable name="ref">
      <xsl:call-template name="ISOCITE"/>
    </xsl:variable>

    <xsl:call-template name="simpleRun">
      <xsl:with-param name="text">
	<xsl:value-of select="translate($ref,' ','&#160;')"/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <xsl:template name="ISOCITE">
    <xsl:value-of select="."/>
    <xsl:for-each select="..">
      <xsl:for-each select="tei:idno[@type='documentType']">
	<xsl:choose>
	  <xsl:when test="contains(../tei:publisher,'/')">
	    <xsl:text> </xsl:text>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:text>/</xsl:text>
	  </xsl:otherwise>
	</xsl:choose>
	<xsl:value-of select="normalize-space(.)"/>
      </xsl:for-each>
      <xsl:text> </xsl:text>
      <xsl:for-each select="tei:idno[@type='docNumber']">
	<xsl:value-of select="normalize-space(.)"/>
      </xsl:for-each>
      
      <xsl:for-each select="tei:idno[@type='parts']">
	<xsl:text> </xsl:text>
	<xsl:value-of select="normalize-space(.)"/>
      </xsl:for-each>
      
      <xsl:for-each select="tei:idno[@type='docPartNumber']">
	<xsl:choose>
	  <xsl:when test=".='(all parts)'">
	    <xsl:text> </xsl:text>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:text>-</xsl:text>
	  </xsl:otherwise>
	</xsl:choose>
	<xsl:value-of select="normalize-space(.)"/>
      </xsl:for-each>
      
      <xsl:for-each select="tei:edition">
	<xsl:text>:</xsl:text>
	<xsl:value-of select="normalize-space(.)"/>
      </xsl:for-each>
      
      <xsl:for-each select="tei:idno[@type='supplType']">
	<xsl:text>/</xsl:text>
	<xsl:value-of select="normalize-space(.)"/>
      </xsl:for-each>
      
      <xsl:for-each select="tei:idno[@type='supplNumber']">
	<xsl:text>.</xsl:text>
	<xsl:value-of select="normalize-space(.)"/>
      </xsl:for-each>
      
      <xsl:for-each select="tei:idno[@type='supplYear']">
	<xsl:text>:</xsl:text>
	<xsl:value-of select="normalize-space(.)"/>
      </xsl:for-each>
      
      <xsl:for-each select="tei:idno[@type='corrType']">
	<xsl:text>/</xsl:text>
	<xsl:value-of select="normalize-space(.)"/>
      </xsl:for-each>
      
      <xsl:for-each select="tei:idno[@type='corrNumber']">
	<xsl:text>.</xsl:text>
	<xsl:value-of select="normalize-space(.)"/>
      </xsl:for-each>
      
      <xsl:for-each select="tei:idno[@type='corrYear']">
	<xsl:text>:</xsl:text>
	<xsl:value-of select="normalize-space(.)"/>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:template>

  <xsl:template
      match="tei:listBibl/tei:bibl/tei:idno[@type='docPartNumber']"/>
  <xsl:template
      match="tei:listBibl/tei:bibl/tei:idno[@type='docNumber']"/>
  <xsl:template
      match="tei:listBibl/tei:bibl/tei:idno[@type='corrNumber']"/>
  <xsl:template
      match="tei:listBibl/tei:bibl/tei:idno[@type='corrType']"/>
  <xsl:template
      match="tei:listBibl/tei:bibl/tei:idno[@type='corrYear']"/>
  <xsl:template
      match="tei:listBibl/tei:bibl/tei:idno[@type='documentType']"/>
  <xsl:template match="tei:listBibl/tei:bibl/tei:idno[@type='parts']"/>
  <xsl:template
      match="tei:listBibl/tei:bibl/tei:idno[@type='supplNumber']"/>
  <xsl:template
      match="tei:listBibl/tei:bibl/tei:idno[@type='supplType']"/>
  <xsl:template
      match="tei:listBibl/tei:bibl/tei:idno[@type='supplYear']"/>
  <xsl:template
      match="tei:listBibl/tei:bibl/tei:edition"/>

  <xsl:template
      match="tei:listBibl/tei:bibl/tei:title">
    <xsl:call-template name="simpleRun">
      <xsl:with-param name="prefix">
	<xsl:if test="../tei:publisher"><!-- its structured -->
	  <xsl:text>, </xsl:text>
	</xsl:if>
      </xsl:with-param>
      <xsl:with-param name="text">
	<xsl:apply-templates/>
      </xsl:with-param>
      <xsl:with-param name="italic">true</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <!-- generic TBX -->
  <xsl:template match="tbx:termEntry">
    <xsl:call-template name="showTermEntry"/>
  </xsl:template>

  <xsl:template name="showTermEntry">
    <xsl:for-each select="tbx:langSet">
      <xsl:call-template name="termNum"/>
      <xsl:for-each select="tbx:ntig">
	<xsl:variable name="Thing">
	  <xsl:value-of select="substring-before(tbx:termGrp/tbx:termNote[@type='administrativeStatus'],'-admn-sts')"/>
	</xsl:variable>
	<xsl:variable name="style">
	  <xsl:choose>
	    <xsl:when test="$Thing='preferredTerm'">termPreferred</xsl:when>
	    <xsl:when test="$Thing='deprecatedTerm'">termDeprecated</xsl:when>
	    <xsl:when test="$Thing='admittedTerm'">termAdmitted</xsl:when>
	    <xsl:when test="$Thing='symbol'">termAdmitted</xsl:when>
	  </xsl:choose>
	</xsl:variable>
	<xsl:call-template name="block-element">
	  <xsl:with-param name="style" select="$style"/>
	</xsl:call-template>
      </xsl:for-each>
      <xsl:apply-templates
	  select="tbx:descripGrp/tbx:descrip[@type='definition']"/>

      <xsl:apply-templates
	  select="tbx:descripGrp/tbx:descrip[@type='figure']"/>

      <xsl:apply-templates
	  select="tbx:descripGrp/tbx:descrip[@type='example']"/>

      <xsl:apply-templates select="tbx:note" mode="tbxnote"/>

      <xsl:apply-templates select="tbx:langSet/tbx:ntig[1]/tbx:note" mode="tbxnote"/>

      <xsl:apply-templates
	  select="tbx:langSet/tbx:ntig[tbx:termGrp/tbx:termNote='symbol-admn-sts']/tbx:note" mode="tbxnote"/>

      <xsl:apply-templates
	  select="tbx:descripGrp[tbx:descrip/@type='definition']/tbx:note" mode="tbxnote"/>

      <xsl:apply-templates
	  select="tbx:descripGrp[tbx:descrip/@type='figure']/tbx:note" mode="tbxnote"/>

      <xsl:apply-templates
	  select="tbx:descripGrp[tbx:descrip/@type='example']/tbx:note" mode="tbxnote"/>

      <xsl:apply-templates select="tbx:descripGrp/tbx:admin[@type='entrySource']"/>
    </xsl:for-each>

      <xsl:apply-templates
	  select="tbx:descripGrp/tbx:descrip[@type='definition']"/>

      <xsl:apply-templates
	  select="tbx:descripGrp/tbx:descrip[@type='figure']"/>

      <xsl:apply-templates
	  select="tbx:descripGrp/tbx:descrip[@type='example']"/>

      <xsl:apply-templates select="tbx:note" mode="tbxnote"/>

      <xsl:apply-templates select="tbx:langSet/tbx:ntig[1]/tbx:note" mode="tbxnote"/>

      <xsl:apply-templates
	  select="tbx:langSet/tbx:ntig[tbx:termGrp/tbx:termNote='symbol-admn-sts']/tbx:note" mode="tbxnote"/>

      <xsl:apply-templates
	  select="tbx:descripGrp[tbx:descrip/@type='definition']/tbx:note" mode="tbxnote"/>

      <xsl:apply-templates
	  select="tbx:descripGrp[tbx:descrip/@type='figure']/tbx:note" mode="tbxnote"/>

      <xsl:apply-templates
	  select="tbx:descripGrp[tbx:descrip/@type='example']/tbx:note" mode="tbxnote"/>

      <xsl:apply-templates select="tbx:descripGrp/tbx:admin[@type='entrySource']"/>

    <xsl:if test="$showTBXMarkup='true'">
      <xsl:call-template name="block-element">
	<xsl:with-param name="select">
	  <egXML xmlns="http://www.tei-c.org/ns/Examples">
	    <xsl:copy-of select="."/>
	  </egXML>
	</xsl:with-param>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>

   <xsl:template
       match="tbx:descripGrp">
     <xsl:apply-templates select="tbx:descrip"/>
   </xsl:template>

   <xsl:template
       match="tbx:ntig">
     <xsl:apply-templates select="tbx:termGrp"/>
   </xsl:template>

   <xsl:template
       match="tbx:descrip[@type='subjectField']"/>

   <xsl:template match="tbx:descrip[@type='example']">
      <xsl:call-template name="block-element">
         <xsl:with-param name="style">Examplenumbered</xsl:with-param>
      </xsl:call-template>

   </xsl:template>

   <xsl:template match="tbx:descrip[@type='figure']">
      <xsl:call-template name="block-element">
         <xsl:with-param name="style">nonVerbalRepresentation</xsl:with-param>
      </xsl:call-template>

   </xsl:template>

   <xsl:template match="tbx:note"/>

   <xsl:template match="tbx:note" mode="tbxnote">
      <xsl:call-template name="block-element">
         <xsl:with-param name="style">
	   <xsl:choose>
	     <xsl:when
		 test="parent::tbx:ntig[tbx:termGrp/tbx:termNote[@type='administrativeStatus'
		       and .='symbol-admn-sts']]">noteSymbol</xsl:when>
	     <xsl:when test="parent::tbx:ntig">noteTerm</xsl:when>
	     <xsl:when test="parent::tbx:termEntry">noteTermEntry</xsl:when>
	     <xsl:when
		 test="parent::tbx:descrip[@type='figure']">noteNonVerbalRepresentation</xsl:when>
	     <xsl:when
		 test="parent::tbx:descrip[@type='example']">noteExample</xsl:when>
	     <xsl:when
		 test="parent::tbx:descrip[@type='definition']">noteDefinition</xsl:when>
	     <xsl:otherwise>noteTermEntry</xsl:otherwise>
	   </xsl:choose>
         </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="tbx:admin[@type='entrySource']">
	 <xsl:variable name="a">
	   <xsl:text>[SOURCE: </xsl:text>
	   <xsl:copy-of select="*|text()"/>
	   <xsl:text>]</xsl:text>
	 </xsl:variable>
	 <xsl:for-each select="$a">
	   <xsl:call-template name="block-element">
	     <xsl:with-param name="style">entrySource</xsl:with-param>
	   </xsl:call-template>
	 </xsl:for-each>
   </xsl:template>
  
  
</xsl:stylesheet>