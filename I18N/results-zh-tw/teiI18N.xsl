<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xpath-default-namespace="http://www.tei-c.org/ns/1.0" version="2.0">

  <xsl:template match="/">
      <html>
          <head>
              <title>Chinese TEI Localization</title>
              <meta name="keywords" content="TEI, Chinese"/>
              <meta name="author" content="Marcus Bingenheimer"/>
              <meta name="date" content="{current-date()}"/>
          </head>
          <body>
              
              <h1>Chinese TEI Localization (<xsl:value-of select="substring-before(string(current-date()),'+')"/>)</h1>
              <p>This is the Chinese localization of the TEI standard. Below please find translations of  definitions of all elements, attributes, attribute values, attribute classes, model classes and macros. <br></br>The Chinese data is also available at the TEI <a href="http://www.tei-c.org">website</a>. In case of discrepancies the current English standard available at the TEI website is authoritative. <br></br>The localization is funded by the National Digital Archive Program 國家數位典藏計畫 and Dharma Drum Buddhist College 法鼓佛教研修學院.
              </p>
              <p>Translators: Wu Tianling 吳恬綾, Huang Weining 黃韋寧<br/>
              Project Directors: Marcus Bingenheimer , Aming Tu</p>
              <ol>
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
              </ol>
              
              
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
        <table border="1">
            <xsl:for-each select="elementSpec">
                <xsl:sort select="@ident"/>
                <tr>
                    <td><table><tr>
                    <td>
                        <b><xsl:text>&lt;</xsl:text>
                        <xsl:value-of select="@ident"/>
                        <xsl:text>&gt;</xsl:text></b>
                    </td>
                        <td></td>
                    </tr>
                    <tr> 
                            <td style="color:blue"> <xsl:value-of select="gloss[not(@xml:lang)]"/></td>
                        <td style="color:blue"> <xsl:value-of select="gloss[@xml:lang='zh-tw']"/></td>
                    </tr>
                        <tr> 
                            <td> <xsl:value-of select="desc[not(@xml:lang)]"/></td>
                            <td> <xsl:value-of select="desc[@xml:lang='zh-tw']"/></td>
                        </tr>
                        <xsl:if test="attList">
                            <tr>
                                <td colspan="2"><table border="1">
                                    <thead><b>Attributes:</b></thead>
                                    <xsl:for-each select="attList/attDef">
                                        <tr><td><span style="color:green"><xsl:value-of select="./@ident"/></span><br/>
                                                      <xsl:value-of select="desc[not(@xml:lang)]"/>
                                            <xsl:if test="valDesc[not(@xml:lang)]">
                                                <br/>(<xsl:value-of select="valDesc[not(@xml:lang)]"/>)
                                            </xsl:if>
                                            <xsl:if test="valList">
                                                <p style="margin-left:5%;">
                                                <br/><span style="color:grey">value list:</span>
                                                <xsl:for-each select="valList/valItem">
                                                    <span style="color:brown;margin-left:5%;"><br/><xsl:value-of select="./@ident"/>
                                                    <br/><xsl:value-of select="gloss[not(@xml:lang)] | desc[not(@xml:lang)]"/></span>
                                                </xsl:for-each>
                                                </p>
                                            </xsl:if>
                                </td>
                                            <td><span style="color:green"><xsl:value-of select="./@ident"/></span><br/>
                                    <xsl:value-of select="desc[@xml:lang='zh-tw']"/>
                                    <xsl:if test="valDesc[@xml:lang='zh-tw']">
                                        <br/>(<xsl:value-of select="valDesc[@xml:lang='zh-tw']"/>)
                                    </xsl:if>
                                    <xsl:if test="valList">
                                        <p style="margin-left:5%;">
                                        <br/><span style="color:grey">value list:</span>
                                        <xsl:for-each select="valList/valItem">
                                            <span style="color:brown;margin-left:5%;"><br/><xsl:value-of select="./@ident"/>
                                            <br/><xsl:value-of select="gloss[@xml:lang='zh-tw'] | desc[@xml:lang='zh-tw']"/></span>
                                        </xsl:for-each>
                                            </p>
                                    </xsl:if>
                                </td>
                                        </tr>
                                        
                                    </xsl:for-each>
                                </table>
                                </td>
                            </tr>
                         </xsl:if>
                </table></td></tr>
            </xsl:for-each>
        </table>
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
                <tr><th colspan="2"><h1><a name="model">3. Model Classes</a></h1><br/>(Members of a given TEI model class share the property that they can all appear in the same location within a document)</th></tr>
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
    
   
    
</xsl:stylesheet>
