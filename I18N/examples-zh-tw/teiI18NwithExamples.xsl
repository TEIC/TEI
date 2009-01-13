<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:teix="http://www.tei-c.org/ns/Examples" xpath-default-namespace="http://www.tei-c.org/ns/1.0"  version="2.0">

  <xsl:template match="/">
      <html>
          <head>
              <title>Chinese TEI Localization</title>
              <meta name="keywords" content="TEI, Chinese"/>
              <meta name="author" content="Marcus Bingenheimer"/>
              <meta name="date" content="{current-date()}"/>
              <style type="text/css">
                  * {background: lemonchiffon;}
                  #intro, #toc {margin-left:5%; margin-right:10%}
                  .gloss {color:lightcoral}
                  p.valueList {margin-left:5%;}
                  .attName {color:green;}
                  div.element {border: black solid; margin-bottom: 3%;}
              </style>
          </head>
          <body>
              
             <div id="intro"> <h1>Chinese TEI Localization (<xsl:value-of select="substring-before(string(current-date()),'+')"/>)</h1>
                 <p>This is the Chinese localization of the TEI standard. Below please find translations of  definitions of all elements, attributes, attribute values, attribute classes, model classes, macros and examples. <br></br>This localization is authorized by the <a href="http://www.tei-c.org">TEI Consortium</a>. It can be accessed in the online version of the <a href="http://tei.oucs.ox.ac.uk/P5/Guidelines-web/zh-tw/html/REF-ELEMENTS.html">guidelines</a> as well as in the documentation output of <a href="http://tei.oucs.ox.ac.uk/Roma/">ROMA</a>. In case of discrepancies, the English standard currently available at the TEI website is authoritative. <br></br>The localization was funded by the National Digital Archive Program 國家數位典藏計畫, the TEI-Consortium and Dharma Drum Buddhist College 法鼓佛教研修學院 from 2006 to 2009.
              </p>
              <p>Translators: Virginia Hsieh 謝筱琳, Wu Tianling 吳恬綾, Huang Weining 黃韋寧.
                  <br/>
              Project Director: Marcus Bingenheimer</p></div>
              <div id="toc"><ol>
                  <li>Modules
                  <ul>
                      <xsl:for-each select="TEI/text/body/moduleSpec">
                          <li><a href="#{@ident}"><xsl:value-of select="@ident"/></a></li>
                      </xsl:for-each>
                  </ul>                     
                  </li>
                  <li><a href="#attribute">Attribute Classes</a></li>
                  <li><a href="#model">Model Classes</a></li>
                  <li><a href="#macro">Macros</a></li>
              </ol></div>
              
              
              <xsl:apply-templates/>
              <br/>
              <xsl:apply-templates mode="classes"/>
          </body>
      </html>
  </xsl:template>
    
    <xsl:template match="teiHeader"/>
    <xsl:template match="teiHeader" mode="classes"/> 
    
    <xsl:template match="moduleSpec">
        <h2><a name="{@ident}"><xsl:number/>. <xsl:value-of select="@ident"/></a></h2>
        <div class="module">
            <xsl:for-each select="elementSpec">
                <xsl:sort select="@ident"/>
                <xsl:variable name="name" select="@ident"/>
                <div class="element"><table>
                    <tr>
                    <td>
                        <b><xsl:text>&lt;</xsl:text>
                        <xsl:value-of select="$name"/>
                        <xsl:text>&gt;</xsl:text></b>
                        <xsl:if test="gloss[@xml:lang='zh-tw'] and gloss[not(@xml:lang)]"><br/>
                        <seg class="gloss">(<xsl:value-of select="gloss[not(@xml:lang)]"/>  <xsl:value-of select="gloss[@xml:lang='zh-tw']"/>)</seg></xsl:if>
                        </td>
                    </tr>
                    <tr class="desc"> 
                           <td> <xsl:value-of select="desc[not(@xml:lang)]"/></td>
                           <td> <xsl:value-of select="desc[@xml:lang='zh-tw']"/></td>
                    </tr>
                        
                        <!-- here is the attribute list -->
                        <xsl:if test="attList">
                            <tr><td colspan="2"><table border="1">
                                <thead><br/><b>Attributes:</b></thead>
                                    <xsl:for-each select="attList/attDef">
                                        <tr><td><span class="attName"><xsl:value-of select="./@ident"/></span><br/>
                                                      <xsl:value-of select="desc[not(@xml:lang)]"/>
                                            <xsl:if test="valDesc[not(@xml:lang)]">
                                                <br/>(<xsl:value-of select="valDesc[not(@xml:lang)]"/>)
                                            </xsl:if>
                                            <xsl:if test="valList">
                                                <p class="valueList">
                                                <br/><span style="color:grey">Value list:</span>
                                                <xsl:for-each select="valList/valItem">
                                                    <span style="color:brown;margin-left:5%;"><br/><xsl:value-of select="./@ident"/>
                                                    <br/><xsl:value-of select="gloss[not(@xml:lang)] | desc[not(@xml:lang)]"/></span>
                                                </xsl:for-each>
                                                </p>
                                            </xsl:if>
                                </td>
                                 <td>
                                     <span style="color:green"><xsl:value-of select="./@ident"/></span><br/>
                                    <xsl:value-of select="desc[@xml:lang='zh-tw']"/>
                                    <xsl:if test="valDesc[@xml:lang='zh-tw']">
                                        <br/>(<xsl:value-of select="valDesc[@xml:lang='zh-tw']"/>)
                                    </xsl:if>
                                    <xsl:if test="valList">
                                        <p class="valueList">
                                        <br/><span style="color:grey">屬性值清單:</span>
                                        <xsl:for-each select="valList/valItem">
                                            <span style="color:brown;"><br/><xsl:value-of select="./@ident"/>
                                            <br/><xsl:value-of select="gloss[@xml:lang='zh-tw'] | desc[@xml:lang='zh-tw']"/></span>
                                        </xsl:for-each>
                                            </p>
                                    </xsl:if>
                                </td></tr>                                        
                                    </xsl:for-each>
                                </table>
                                </td>
                            </tr>
                        </xsl:if>
                        
                        <!-- Here come the examples -->
                        <xsl:if test="document('../chinTEIexamples/allexamples.xml')/TEI/elementSpec[@ident= $name]">                            
                            <tr><td colspan="2"><table border="1">
                                <thead><br/><b>Example(s):</b></thead>                                
                                <xsl:for-each select="document('../chinTEIexamples/allexamples.xml')/TEI/elementSpec[@ident= $name]/teix:egXML[not(@xml:lang)]">
                                    <xsl:variable name="position" select="position()"/>
                                    <tr>
                                        <td style="background:lavender; width:45%;">
                                            <xsl:apply-templates select=".[not(@xml:lang)]"/>
                                        </td>
                                        <td style="background:beige; width:45%;">
                                            <xsl:apply-templates select="following-sibling::teix:egXML[@xml:lang][1]"/>
                                           <!--   <xsl:if test="following-sibling::teix:egXML[@xml:lang][position() = $position]/following-sibling::note">
                                                <p class="note"><xsl:value-of select="following-sibling::teix:egXML[@xml:lang][position()= $position]/following-sibling::note"/></p>
                                                </xsl:if>  -->
                                        </td>
                                    </tr>
                                </xsl:for-each>
                            </table></td></tr>
                        </xsl:if>
                </table></div>
            </xsl:for-each>
            </div>
    </xsl:template>
    
    <xsl:template match="/TEI/text/body" mode="classes">
        <xsl:if test="moduleSpec/classSpec[@type='atts']">
            <table>
                <tr><th colspan="2"><h1><a name="attribute">2. Attribute Classes</a></h1><br/>(Attribute classes group together elements which share some set of common attributes)</th></tr>
            <xsl:for-each select="//classSpec[@type='atts']">
                <xsl:sort select="@ident"/>
                <tr>
                    <td><xsl:value-of select="@ident"/><br/><xsl:value-of select="desc[not(@xml:lang)]"/></td>
                    <td><br/><xsl:value-of select="desc[@xml:lang]"/></td>
                </tr>
            </xsl:for-each>
            </table>
        </xsl:if>
        
        <xsl:if test="moduleSpec/classSpec[@type='model']">            
            <table>
                <tr><th colspan="2"><h1><a name="model">3. Model Classes</a></h1><br/>(Members of a TEI model class share the property that they can all appear in the same location within a document)</th></tr>
                <xsl:for-each select="//classSpec[@type='model']">
                    <xsl:sort select="@ident"/>
                    <tr>
                        <td><xsl:value-of select="@ident"/><br/><xsl:value-of select="desc[not(@xml:lang)]"/></td>
                        <td><br/><xsl:value-of select="desc[@xml:lang]"/></td>
                    </tr>
                </xsl:for-each>
            </table>
        </xsl:if>
        
        <xsl:if test="moduleSpec/macroSpec">            
            <table>
                <tr><th colspan="2"><h1><a name="macro">4. Macros</a></h1><br/>(Shortcut names for frequently occurring parts of other declarations)</th></tr>
                <xsl:for-each select="//macroSpec">
                    <xsl:sort select="@ident"/>
                    <tr>
                        <td><xsl:value-of select="@ident"/><br/><xsl:value-of select="desc[not(@xml:lang)]"/></td>
                        <td><br/><xsl:value-of select="desc[@xml:lang]"/></td>
                    </tr>
                </xsl:for-each>
            </table>
        </xsl:if>        
    </xsl:template>
    
    
    <!-- These two templates output the element structure in the English examples in "allexamples.xml". Some examples involve nesting of many layers, but "teix:egXML[not(@xml:lang)]//*" takes care of iteration. -->
    <xsl:template match="teix:egXML[not(@xml:lang)]">
        <xsl:apply-templates/>
    </xsl:template>
    
    <xsl:template match="teix:egXML[not(@xml:lang)]//*">
        <xsl:if test="preceding-sibling::*"><br/></xsl:if>&lt;<xsl:value-of select="name()"/> 
        <xsl:for-each select="attribute::*">
            <xsl:text> </xsl:text><xsl:value-of select="name()"/><xsl:text>="</xsl:text>
            <xsl:value-of select="."/><xsl:text>"</xsl:text>
        </xsl:for-each>
        <xsl:choose> 
            <xsl:when test="text()">&gt;</xsl:when>
            <xsl:otherwise></xsl:otherwise>
        </xsl:choose>        
        <xsl:apply-templates/> 
        <!-- This in case the content is a comment as in interpGrp-->
        <xsl:if test="comment()">
            <xsl:text>&lt;!-- </xsl:text><xsl:value-of select="comment()"/><xsl:text>--></xsl:text>
        </xsl:if>
        <xsl:choose> 
            <xsl:when test="text()">&lt;/<xsl:value-of select="name()"/>&gt;</xsl:when>
            <xsl:otherwise>/&gt;</xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
    <!-- These two templates output the element structure in the Chinese examples. Some examples involve nesting of many layers, but "teix:egXML[not(@xml:lang)]//*" takes care of iteration -->
    <xsl:template match="teix:egXML[@xml:lang]">
        <xsl:apply-templates/>
    </xsl:template>
    
    <xsl:template match="teix:egXML[@xml:lang]//*">
        <xsl:if test="preceding-sibling::*"><br/></xsl:if>&lt;<xsl:value-of select="name()"/> 
        <xsl:for-each select="attribute::*">
            <xsl:text> </xsl:text><xsl:value-of select="name()"/><xsl:text>="</xsl:text>
            <xsl:value-of select="."/><xsl:text>"</xsl:text>
        </xsl:for-each>
        <xsl:choose> 
            <xsl:when test="text()">&gt;</xsl:when>
            <xsl:otherwise></xsl:otherwise>
        </xsl:choose>        
        <xsl:apply-templates/> 
        <!-- This in case the content is a comment as in interpGrp-->
        <xsl:if test="comment()">
            <xsl:text>&lt;!-- </xsl:text><xsl:value-of select="comment()"/><xsl:text>--></xsl:text>
        </xsl:if>
        <xsl:choose> 
            <xsl:when test="text()">&lt;/<xsl:value-of select="name()"/>&gt;</xsl:when>
            <xsl:otherwise>/&gt;</xsl:otherwise>
        </xsl:choose>
    </xsl:template>
   
    
</xsl:stylesheet>
