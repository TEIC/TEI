<xsl:stylesheet 
 xmlns:teix="http://www.tei-c.org/ns/Examples"
 xmlns:tei="http://www.tei-c.org/ns/1.0"
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 xmlns:rng="http://relaxng.org/ns/structure/1.0"
 extension-element-prefixes="exsl"
 exclude-result-prefixes="exsl teix rng tei"
 xmlns:exsl="http://exslt.org/common"
 version="1.0">

<xsl:output 
   method="xml"
   indent="yes"
   cdata-section-elements="tei:eg teix:egXML"
   omit-xml-declaration="yes"/>

<xsl:template match="comment()[contains(.,'Copyright 2004 TEI')]"/>

<xsl:template match="comment()[contains(.,'Copyright TEI')]"/>

<xsl:template match="teix:*|tei:*|rng:*">
 <xsl:copy>
  <xsl:apply-templates select="@*"/>
  <xsl:apply-templates 
      select="teix:*|tei:*|rng:*|comment()|processing-instruction()|text()"/>
 </xsl:copy>
</xsl:template>







<xsl:template match="tei:interaction//tei:attDef[@ident='active']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:relation//tei:attDef[@ident='active']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointers"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:node//tei:attDef[@ident='adj']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointers"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:node//tei:attDef[@ident='adjFrom']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointers"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:node//tei:attDef[@ident='adjTo']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointers"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:person//tei:attDef[@ident='age']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.duration"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:personGrp//tei:attDef[@ident='age']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.duration"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:damage//tei:attDef[@ident='agent']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:gap//tei:attDef[@ident='agent']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:supplied//tei:attDef[@ident='agent']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:unclear//tei:attDef[@ident='agent']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='ana']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointers"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:note//tei:attDef[@ident='anchored']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.uboolean"/>
  </xsl:copy>
</xsl:template>

 

 <xsl:template match="tei:certainty//tei:attDef[@ident='assertedValue']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token | tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:specDesc//tei:attDef[@ident='atts']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.tokens"/>
  </xsl:copy>
</xsl:template>

 

 <xsl:template match="tei:fsDecl//tei:attDef[@ident='baseType']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:date//tei:attDef[@ident='calendar']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:dateRange//tei:attDef[@ident='calendar']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:dateStruct//tei:attDef[@ident='calendar']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='cause']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:add//tei:attDef[@ident='cert']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.certainty"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:addSpan//tei:attDef[@ident='cert']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.certainty"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:corr//tei:attDef[@ident='cert']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.certainty"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:del//tei:attDef[@ident='cert']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.certainty"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:delSpan//tei:attDef[@ident='cert']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.certainty"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:expan//tei:attDef[@ident='cert']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.certainty"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:restore//tei:attDef[@ident='cert']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.certainty"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:unclear//tei:attDef[@ident='cert']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.certainty"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='certainty']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.certainty"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:date//tei:attDef[@ident='certainty']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.certainty"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:hand//tei:attDef[@ident='character']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:handShift//tei:attDef[@ident='character']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:iNode//tei:attDef[@ident='children']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointers"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:root//tei:attDef[@ident='children']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointers"/>
  </xsl:copy>
</xsl:template>

 

 <xsl:template match="tei:msContents//tei:attDef[@ident='class']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:msItem//tei:attDef[@ident='class']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:msItemStruct//tei:attDef[@ident='class']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:occupation//tei:attDef[@ident='code']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:socecStatus//tei:attDef[@ident='code']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 

 

 <xsl:template match="tei:layout//tei:attDef[@ident='columns']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.numeric"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:macroSpec//tei:attDef[@ident='combine']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.uboolean??"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:binding//tei:attDef[@ident='contemporary']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.uboolean"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:seal//tei:attDef[@ident='contemporary']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.uboolean"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='copyOf']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='corresp']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointers"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:teiHeader//tei:attDef[@ident='creator']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:gloss//tei:attDef[@ident='cref']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.tokens"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:ptr//tei:attDef[@ident='cref']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.tokens"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:ref//tei:attDef[@ident='cref']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.tokens"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:term//tei:attDef[@ident='cref']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.tokens"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:birth//tei:attDef[@ident='date']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.temporalExpression"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='dateAttrib']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:teiHeader//tei:attDef[@ident='dateCreated']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.temporalExpression"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:teiHeader//tei:attDef[@ident='dateUpdated']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.temporalExpression"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='decls']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointers"/>
  </xsl:copy>
</xsl:template>

 

 <xsl:template match="tei:explicit//tei:attDef[@ident='defective']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.uboolean"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:incipit//tei:attDef[@ident='defective']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.uboolean"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:msContents//tei:attDef[@ident='defective']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.uboolean"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:msItem//tei:attDef[@ident='defective']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.uboolean"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:msItemStruct//tei:attDef[@ident='defective']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.uboolean"/>
  </xsl:copy>
</xsl:template>

 

 <xsl:template match="tei:damage//tei:attDef[@ident='degree']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.certainty?"/>
  </xsl:copy>
</xsl:template>

 

 <xsl:template match="tei:purpose//tei:attDef[@ident='degree']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.certainty"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:state//tei:attDef[@ident='delim']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.tokens"/>
  </xsl:copy>
</xsl:template>

 

 <xsl:template match="tei:certainty//tei:attDef[@ident='desc']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:event//tei:attDef[@ident='desc']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 

 

 

 <xsl:template match="tei:kinesic//tei:attDef[@ident='desc']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:relation//tei:attDef[@ident='desc']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:respons//tei:attDef[@ident='desc']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:restore//tei:attDef[@ident='desc']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:vocal//tei:attDef[@ident='desc']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:space//tei:attDef[@ident='dim']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:q//tei:attDef[@ident='direct']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.uboolean"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:sound//tei:attDef[@ident='discrete']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.uboolean"/>
  </xsl:copy>
</xsl:template>

 

 

 <xsl:template match="tei://tei:attDef[@ident='dur']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.duration"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:recording//tei:attDef[@ident='dur']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.duration"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:cb//tei:attDef[@ident='ed']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:lb//tei:attDef[@ident='ed']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:milestone//tei:attDef[@ident='ed']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:pb//tei:attDef[@ident='ed']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:state//tei:attDef[@ident='ed']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='end']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='enjamb']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:hyphenation//tei:attDef[@ident='eol']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='evaluate']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='evidence']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 

 <xsl:template match="tei:dateStruct//tei:attDef[@ident='exact']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.certainty?"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:distance//tei:attDef[@ident='exact']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 

 <xsl:template match="tei://tei:attDef[@ident='exclude']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointers"/>
  </xsl:copy>
</xsl:template>

 

 <xsl:template match="tei:damage//tei:attDef[@ident='extent']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.tokens"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:gap//tei:attDef[@ident='extent']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.tokens"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:orth//tei:attDef[@ident='extent']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.tokens"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:pron//tei:attDef[@ident='extent']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.tokens"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:space//tei:attDef[@ident='extent']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.tokens"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:fs//tei:attDef[@ident='feats']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointers"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:shift//tei:attDef[@ident='feature']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:hand//tei:attDef[@ident='first']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.uboolean"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:iNode//tei:attDef[@ident='follow']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:leaf//tei:attDef[@ident='follow']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:biblItem//tei:attDef[@ident='form']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:objectDesc//tei:attDef[@ident='form']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:quotation//tei:attDef[@ident='form']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:app//tei:attDef[@ident='from']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:arc//tei:attDef[@ident='from']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:dateRange//tei:attDef[@ident='from']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.temporalExpression"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:locus//tei:attDef[@ident='from']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:span//tei:attDef[@ident='from']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:timeRange//tei:attDef[@ident='from']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.temporalExpression"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='full']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='full']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='function']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:f//tei:attDef[@ident='fVal']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:tagUsage//tei:attDef[@ident='gi']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:certainty//tei:attDef[@ident='given']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointers"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:writing//tei:attDef[@ident='gradual']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.uboolean"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='hand']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:add//tei:attDef[@ident='hand']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:addSpan//tei:attDef[@ident='hand']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:damage//tei:attDef[@ident='hand']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:del//tei:attDef[@ident='hand']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:delSpan//tei:attDef[@ident='hand']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:gap//tei:attDef[@ident='hand']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 

 <xsl:template match="tei:restore//tei:attDef[@ident='hand']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:supplied//tei:attDef[@ident='hand']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:unclear//tei:attDef[@ident='hand']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:handDesc//tei:attDef[@ident='hands']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:graphic//tei:attDef[@ident='height']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.numeric"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='ident']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:language//tei:attDef[@ident='ident']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.language"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:witness//tei:attDef[@ident='included']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.tokens"/>
  </xsl:copy>
</xsl:template>

 

 

 <xsl:template match="tei:hand//tei:attDef[@ident='ink']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:handShift//tei:attDef[@ident='ink']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='inst']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointers"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:timeline//tei:attDef[@ident='interval']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.numeric"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:when//tei:attDef[@ident='interval']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.numeric"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:event//tei:attDef[@ident='iterated']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.uboolean"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:kinesic//tei:attDef[@ident='iterated']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.uboolean"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:vocal//tei:attDef[@ident='iterated']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.uboolean"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='key']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='key']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='key']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='key']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token"/>
  </xsl:copy>
</xsl:template>

 

 

 

 <xsl:template match="tei:orgName//tei:attDef[@ident='key']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token"/>
  </xsl:copy>
</xsl:template>

 

 

 

 

 

 

 

 

 

 

 

 

 

 <xsl:template match="tei:textLang//tei:attDef[@ident='langKey']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.language"/>
  </xsl:copy>
</xsl:template>

 

 

 

 

 

 

 <xsl:template match="tei:sense//tei:attDef[@ident='level']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.numeric"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:title//tei:attDef[@ident='level']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:app//tei:attDef[@ident='loc']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.tokens"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='location']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:variantEncoding//tei:attDef[@ident='location']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:certainty//tei:attDef[@ident='locus']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:respons//tei:attDef[@ident='locus']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:hand//tei:attDef[@ident='mainLang']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.language"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:quotation//tei:attDef[@ident='marks']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:supportDesc//tei:attDef[@ident='material']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:numeric//tei:attDef[@ident='max']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.numeric"/>
  </xsl:copy>
</xsl:template>

 

 <xsl:template match="tei://tei:attDef[@ident='mergedin']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='met']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.tokens"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:correction//tei:attDef[@ident='method']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:normalization//tei:attDef[@ident='method']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:variantEncoding//tei:attDef[@ident='method']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='mode']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:alt//tei:attDef[@ident='mode']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:altGrp//tei:attDef[@ident='mode']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:channel//tei:attDef[@ident='mode']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 

 <xsl:template match="tei:relation//tei:attDef[@ident='mutual']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.uboolean"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='n']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.tokens"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:equiv//tei:attDef[@ident='name']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.tokens"/>
  </xsl:copy>
</xsl:template>

 

 

 <xsl:template match="tei:vLabel//tei:attDef[@ident='name']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token"/>
  </xsl:copy>
</xsl:template>

 

 <xsl:template match="tei:handShift//tei:attDef[@ident='new']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:shift//tei:attDef[@ident='new']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='next']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='norm']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.tokens"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='notAfter']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.temporalExpression"/>
  </xsl:copy>
</xsl:template>

 

 <xsl:template match="tei:pron//tei:attDef[@ident='notation']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='notBefore']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.temporalExpression"/>
  </xsl:copy>
</xsl:template>

 

 

 <xsl:template match="tei:handShift//tei:attDef[@ident='old']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='opt']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.uboolean"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:iNode//tei:attDef[@ident='ord']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.uboolean"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:root//tei:attDef[@ident='ord']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.uboolean"/>
  </xsl:copy>
</xsl:template>

 

 

 

 <xsl:template match="tei://tei:attDef[@ident='org']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:attList//tei:attDef[@ident='org']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:coll//tei:attDef[@ident='org']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:fDecl//tei:attDef[@ident='org']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:vMerge//tei:attDef[@ident='org']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 

 <xsl:template match="tei:timeline//tei:attDef[@ident='origin']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:textLang//tei:attDef[@ident='otherLangs']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.tokens"/>
  </xsl:copy>
</xsl:template>

 

 

 

 <xsl:template match="tei:iNode//tei:attDef[@ident='parent']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:leaf//tei:attDef[@ident='parent']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='part']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='part']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:ab//tei:attDef[@ident='part']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:l//tei:attDef[@ident='part']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:interaction//tei:attDef[@ident='passive']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:relation//tei:attDef[@ident='passive']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 

 <xsl:template match="tei:metDecl//tei:attDef[@ident='pattern']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.regexp"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:move//tei:attDef[@ident='perf']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointers"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:tech//tei:attDef[@ident='perf']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointers"/>
  </xsl:copy>
</xsl:template>

 

 

 <xsl:template match="tei:fw//tei:attDef[@ident='place']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:note//tei:attDef[@ident='place']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:witDetail//tei:attDef[@ident='place']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 

 <xsl:template match="tei://tei:attDef[@ident='prev']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:fragmentPattern//tei:attDef[@ident='re']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.regexp"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='real']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.tokens"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:gap//tei:attDef[@ident='reason']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.tokens"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:supplied//tei:attDef[@ident='reason']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.tokens"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:unclear//tei:attDef[@ident='reason']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.tokens"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:g//tei:attDef[@ident='ref']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 

 

 

 

 

 

 

 <xsl:template match="tei://tei:attDef[@ident='rend']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.tokens"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:tagUsage//tei:attDef[@ident='render']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='resp']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='resp']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:add//tei:attDef[@ident='resp']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:addSpan//tei:attDef[@ident='resp']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:corr//tei:attDef[@ident='resp']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:damage//tei:attDef[@ident='resp']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:del//tei:attDef[@ident='resp']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:delSpan//tei:attDef[@ident='resp']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:expan//tei:attDef[@ident='resp']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:gap//tei:attDef[@ident='resp']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:hand//tei:attDef[@ident='resp']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:handShift//tei:attDef[@ident='resp']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:note//tei:attDef[@ident='resp']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:reg//tei:attDef[@ident='resp']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:respons//tei:attDef[@ident='resp']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:restore//tei:attDef[@ident='resp']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:space//tei:attDef[@ident='resp']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:supplied//tei:attDef[@ident='resp']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:unclear//tei:attDef[@ident='resp']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:witDetail//tei:attDef[@ident='resp']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 

 

 <xsl:template match="tei://tei:attDef[@ident='rhyme']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.tokens"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:biblItem//tei:attDef[@ident='role']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:cell//tei:attDef[@ident='role']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:editor//tei:attDef[@ident='role']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:person//tei:attDef[@ident='role']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:personGrp//tei:attDef[@ident='role']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:row//tei:attDef[@ident='role']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 

 

 <xsl:template match="tei:layout//tei:attDef[@ident='ruledLines']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.numeric"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='sameAs']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='sample']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:graphic//tei:attDef[@ident='scale']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.numeric"/>
  </xsl:copy>
</xsl:template>

 

 <xsl:template match="tei:catRef//tei:attDef[@ident='scheme']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:classCode//tei:attDef[@ident='scheme']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 

 <xsl:template match="tei:keywords//tei:attDef[@ident='scheme']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:locus//tei:attDef[@ident='scheme']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:occupation//tei:attDef[@ident='scheme']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:socecStatus//tei:attDef[@ident='scheme']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 

 <xsl:template match="tei://tei:attDef[@ident='scope']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:handNote//tei:attDef[@ident='scope']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:join//tei:attDef[@ident='scope']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:hand//tei:attDef[@ident='scribe']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 

 

 

 <xsl:template match="tei://tei:attDef[@ident='select']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointers"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:person//tei:attDef[@ident='sex']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.sex"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:personGrp//tei:attDef[@ident='sex']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.sex"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:witness//tei:attDef[@ident='sigil']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:when//tei:attDef[@ident='since']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 

 <xsl:template match="tei:personGrp//tei:attDef[@ident='size']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.tokens"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:distinct//tei:attDef[@ident='social']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 

 <xsl:template match="tei:normalization//tei:attDef[@ident='source']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:supplied//tei:attDef[@ident='source']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.tokens"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:distinct//tei:attDef[@ident='space']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 

 <xsl:template match="tei://tei:attDef[@ident='start']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 

 <xsl:template match="tei:availability//tei:attDef[@ident='status']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:biblItem//tei:attDef[@ident='status']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:correction//tei:attDef[@ident='status']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.certainty"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:del//tei:attDef[@ident='status']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:delSpan//tei:attDef[@ident='status']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:msDescription//tei:attDef[@ident='status']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 

 <xsl:template match="tei:hand//tei:attDef[@ident='style']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:handShift//tei:attDef[@ident='style']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='subtype']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:decoNote//tei:attDef[@ident='subtype']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:seg//tei:attDef[@ident='subtype']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='synch']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointers"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='target']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:catRef//tei:attDef[@ident='target']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointers"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:certainty//tei:attDef[@ident='target']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointers"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:gloss//tei:attDef[@ident='target']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:note//tei:attDef[@ident='target']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointers"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:ptr//tei:attDef[@ident='target']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointers"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:ref//tei:attDef[@ident='target']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointers"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:respons//tei:attDef[@ident='target']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointers"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:specGrpRef//tei:attDef[@ident='target']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:term//tei:attDef[@ident='target']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:witDetail//tei:attDef[@ident='target']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointers"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:note//tei:attDef[@ident='targetEnd']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointers"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:alt//tei:attDef[@ident='targets']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointers"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:join//tei:attDef[@ident='targets']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointers"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:link//tei:attDef[@ident='targets']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointers"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:locus//tei:attDef[@ident='targets']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointers"/>
  </xsl:copy>
</xsl:template>

 

 

 

 <xsl:template match="tei:distinct//tei:attDef[@ident='time']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:addSpan//tei:attDef[@ident='to']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:app//tei:attDef[@ident='to']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:arc//tei:attDef[@ident='to']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:dateRange//tei:attDef[@ident='to']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.temporalExpression"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:delSpan//tei:attDef[@ident='to']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:locus//tei:attDef[@ident='to']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:span//tei:attDef[@ident='to']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:timeRange//tei:attDef[@ident='to']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.temporalExpression"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:u//tei:attDef[@ident='trans']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 

 <xsl:template match="tei://tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:abbr//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:accMat//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:addSpan//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:altName//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:app//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:biblScope//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:camera//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:castItem//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:classSpec//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:code//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:colloc//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:constitution//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:custEvent//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:damage//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:decoNote//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:del//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:delSpan//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:derivation//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:dimensions//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:distinct//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:divGen//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:domain//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:explicit//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:finalRubric//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:forest//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:forestGrp//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:form//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:fs//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:fsdDecl//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:fsDecl//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:fvLib//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:fw//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:geogName//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:gram//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:graph//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:head//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:idno//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:incipit//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:interaction//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:itype//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:lbl//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:list//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:macroSpec//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:measure//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 

 <xsl:template match="tei:moduleSpec//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:move//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:msDescription//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:name//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:node//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:note//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:num//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:oRef//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:orgDivn//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:orgName//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:orgTitle//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:orgType//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:orth//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:oVar//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:persName//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:preparedness//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:purpose//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:q//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:re//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:recording//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:relation//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 

 <xsl:template match="tei:rs//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:rubric//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:schemaSpec//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:sound//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:stage//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:tech//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:teiHeader//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:term//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:time//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:title//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:titlePage//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:titlePart//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:usg//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:valList//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:witDetail//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:writing//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:xr//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.token via tei.attr.typed"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:factuality//tei:attDef[@ident='type']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:milestone//tei:attDef[@ident='unit']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:state//tei:attDef[@ident='unit']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:timeline//tei:attDef[@ident='unit']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:when//tei:attDef[@ident='unit']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='units']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:equiv//tei:attDef[@ident='uri']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:normalization//tei:attDef[@ident='uri']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:fsdDecl//tei:attDef[@ident='url']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:graphic//tei:attDef[@ident='url']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:moduleRef//tei:attDef[@ident='url']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:attDef//tei:attDef[@ident='usage']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:elementSpec//tei:attDef[@ident='usage']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:language//tei:attDef[@ident='usage']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.probability"/>
  </xsl:copy>
</xsl:template>

 

 <xsl:template match="tei://tei:attDef[@ident='value']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.temporalExpression"/>
  </xsl:copy>
</xsl:template>

 

 <xsl:template match="tei:date//tei:attDef[@ident='value']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.temporalExpression"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:docDate//tei:attDef[@ident='value']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.temporalExpression"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:eLeaf//tei:attDef[@ident='value']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:eTree//tei:attDef[@ident='value']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:iNode//tei:attDef[@ident='value']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 

 <xsl:template match="tei:leaf//tei:attDef[@ident='value']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:metSym//tei:attDef[@ident='value']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.string"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:node//tei:attDef[@ident='value']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:num//tei:attDef[@ident='value']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.numeric"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:numeric//tei:attDef[@ident='value']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.numeric"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:root//tei:attDef[@ident='value']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 

 

 <xsl:template match="tei:time//tei:attDef[@ident='value']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.temporalExpression"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:triangle//tei:attDef[@ident='value']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 

 

 <xsl:template match="tei:unicodeName//tei:attDef[@ident='version']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.numeric"/>
  </xsl:copy>
</xsl:template>

 

 

 <xsl:template match="tei:event//tei:attDef[@ident='who']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:kinesic//tei:attDef[@ident='who']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:move//tei:attDef[@ident='who']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointers"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:pause//tei:attDef[@ident='who']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:q//tei:attDef[@ident='who']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.code"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:setting//tei:attDef[@ident='who']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointers"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:shift//tei:attDef[@ident='who']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:sp//tei:attDef[@ident='who']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointers"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:u//tei:attDef[@ident='who']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointers"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:vocal//tei:attDef[@ident='who']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:writing//tei:attDef[@ident='who']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer"/>
  </xsl:copy>
</xsl:template>

 

 <xsl:template match="tei://tei:attDef[@ident='wit']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.tokens"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei://tei:attDef[@ident='wit']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.tokens"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:witDetail//tei:attDef[@ident='wit']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.tokens"/>
  </xsl:copy>
</xsl:template>

 

 <xsl:template match="tei:layout//tei:attDef[@ident='writtenLines']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.numeric"/>
  </xsl:copy>
</xsl:template>

 

 

 <xsl:template match="tei://tei:attDef[@ident='xmlbase']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.pointer?"/>
  </xsl:copy>
</xsl:template>

 

 <xsl:template match="tei://tei:attDef[@ident='xmllang']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.language"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:time//tei:attDef[@ident='zone']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>

 <xsl:template match="tei:timeStruct//tei:attDef[@ident='zone']/tei:datatype">
  <xsl:copy>
    <rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0" name="tei.data.enumerated"/>
  </xsl:copy>
</xsl:template>


 
</xsl:stylesheet>
