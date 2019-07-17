<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:math="http://www.w3.org/2005/xpath-functions/math"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:j="http://www.w3.org/2005/xpath-functions"
    exclude-result-prefixes="xs math"
    version="3.0">
    
    <xsl:import href="../common/functions.xsl"/>
    <xsl:key match="tei:elementSpec|tei:classSpec|tei:macroSpec|tei:dataSpec" name="IDENTS" use="concat(@prefix,@ident)"/>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
        <desc>
            <p> TEI stylesheet for making JSON from ODD </p>
            <p>This software is dual-licensed:3
                
                1. Distributed under a Creative Commons Attribution-ShareAlike 3.0
                Unported License http://creativecommons.org/licenses/by-sa/3.0/ 
                
                2. http://www.opensource.org/licenses/BSD-2-Clause
                
                
                
                Redistribution and use in source and binary forms, with or without
                modification, are permitted provided that the following conditions are
                met:
                
                * Redistributions of source code must retain the above copyright
                notice, this list of conditions and the following disclaimer.
                
                * Redistributions in binary form must reproduce the above copyright
                notice, this list of conditions and the following disclaimer in the
                documentation and/or other materials provided with the distribution.
                
                This software is provided by the copyright holders and contributors
                "as is" and any express or implied warranties, including, but not
                limited to, the implied warranties of merchantability and fitness for
                a particular purpose are disclaimed. In no event shall the copyright
                holder or contributors be liable for any direct, indirect, incidental,
                special, exemplary, or consequential damages (including, but not
                limited to, procurement of substitute goods or services; loss of use,
                data, or profits; or business interruption) however caused and on any
                theory of liability, whether in contract, strict liability, or tort
                (including negligence or otherwise) arising in any way out of the use
                of this software, even if advised of the possibility of such damage.
            </p> 
            <p>Author: See AUTHORS</p>
            
            <p>Copyright: 2019, TEI Consortium</p>
        </desc>
    </doc>
    
    <xsl:output method="text"/>
    
    <xsl:param name="serializeDocs" select="true()"/>
    <xsl:param name="defaultTEIServer">http://www.tei-c.org/Vault/P5/</xsl:param>
    <xsl:param name="defaultTEIVersion">current</xsl:param>
    <xsl:param name="defaultSource"/>
    <xsl:param name="configDirectory"/>
    <xsl:param name="currentDirectory"/>
    <xsl:param name="verbose">false</xsl:param>
    <xsl:param name="doclang" select="'en'"/>
    
    <xsl:variable name="DEFAULTSOURCE">
        <xsl:choose>
            <xsl:when test="$defaultSource != ''">
                <xsl:choose>
                    <xsl:when test="starts-with($defaultSource, '&quot;') and ends-with($defaultSource, '&quot;')">
                        <xsl:value-of select="substring($defaultSource, 2, string-length($defaultSource)-2)"/>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:value-of select="$defaultSource"/>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:when>
            <xsl:when test="$configDirectory != ''">
                <xsl:value-of select="$configDirectory"/>
                <xsl:text>odd/p5subset.xml</xsl:text>
            </xsl:when>
            <xsl:otherwise>
                <xsl:value-of select="$defaultTEIServer"/>
                <xsl:value-of select="$defaultTEIVersion"/>
                <xsl:text>/xml/tei/odd/p5subset.xml</xsl:text>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:variable>
    
    <xsl:function name="tei:workOutSource" as="xs:string*">
        <xsl:param name="e"/>
        <xsl:variable name="loc">
            <xsl:choose>
                <xsl:when test="$e/@source">
                    <xsl:value-of select="$e/@source"/>
                </xsl:when>
                <xsl:when test="$e/ancestor::tei:schemaSpec/@source">
                    <xsl:value-of select="$e/ancestor::tei:schemaSpec/@source"/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="$DEFAULTSOURCE"/>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        <xsl:variable name="source">
            <xsl:choose>
                <xsl:when test="starts-with($loc,'file:')">
                    <xsl:value-of select="$loc"/>
                </xsl:when>
                <xsl:when test="starts-with($loc,'http:')">
                    <xsl:value-of select="$loc"/>
                </xsl:when>
                <xsl:when test="starts-with($loc,'https:')">
                    <xsl:value-of select="$loc"/>
                </xsl:when>
                <xsl:when test="starts-with($loc,'/')">
                    <xsl:value-of select="resolve-uri($loc, 'file:///')"/>
                </xsl:when>
                <xsl:when test="starts-with($loc,'tei:')">
                    <xsl:value-of select="replace($loc,'tei:',$defaultTEIServer)"/>
                    <xsl:text>/xml/tei/odd/p5subset.xml</xsl:text>
                </xsl:when>
                <xsl:when test="base-uri($top)=''">
                    <xsl:value-of select="$currentDirectory"/>
                    <xsl:value-of select="$loc"/>
                </xsl:when>
                <xsl:when test="$currentDirectory=''">
                    <xsl:value-of select="resolve-uri($loc,base-uri($top))"/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="resolve-uri(string-join(($currentDirectory, $loc), '/'),base-uri($top))"/>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="not(doc-available($source))">
                <!-- noop -->
            </xsl:when>
            <xsl:otherwise>
                <xsl:if test="$verbose='true'">
                    <xsl:message>Setting source document to <xsl:value-of select="$source"/></xsl:message>
                </xsl:if>
                <xsl:sequence select="$source"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:function>    
    
    <xsl:function name="tei:getClassType" as="xs:string">
        <xsl:param name="context"/>
        <xsl:param name="key"/>
        <xsl:for-each select="$context">
            <xsl:choose>
                <xsl:when test="key('IDENTS', $key)/@type">
                    <xsl:value-of select="key('IDENTS', $key)/@type"/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="document(tei:workOutSource($context))//tei:classSpec[@ident=$key]/@type"/>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:for-each>
    </xsl:function>
    
    <xsl:template match="/">        
        <xsl:variable name="structure">
            <j:map>
                <j:string key="title">
                    <xsl:sequence select="tei:generateMetadataTitle(*)"/>
                </j:string>
                <j:string key="edition">
                    <xsl:sequence select="tei:generateEdition(*)"/>
                </j:string>
                <j:string key="generator">odd2json3</j:string>
                <j:string key="date"><xsl:sequence select="tei:whatsTheDate()"/></j:string>
                <j:array key="modules">
                    <xsl:for-each select="//tei:moduleSpec">
                        <xsl:sort select="@ident"/>
                        <j:map>
                            <j:string key="ident"><xsl:value-of select="@ident"/></j:string>
                            <j:string key="id">
                                <xsl:choose>
                                    <xsl:when test="@n">
                                        <xsl:value-of select="@n">
                                        </xsl:value-of>
                                    </xsl:when>
                                    <xsl:otherwise>
                                        <xsl:value-of select="ancestor::tei:div[last()]/@xml:id"/>
                                    </xsl:otherwise>
                                </xsl:choose>
                            </j:string>
                            <xsl:call-template name="desc"/>
                            <j:array key="altIdent">
                                <xsl:for-each select="tei:altIdent">
                                    <j:string><xsl:value-of select="."/></j:string>
                                </xsl:for-each>                                
                            </j:array>
                        </j:map>
                    </xsl:for-each>
                </j:array>
                <j:array key="moduleRefs">
                    <xsl:for-each select="//tei:moduleRef">
                        <xsl:sort select="@key"/>
                        <j:map>
                            <j:string key="key"><xsl:value-of select="@key"/></j:string>
                            <xsl:call-template name="desc"/>
                            <xsl:call-template name="mode"/>
                        </j:map>
                    </xsl:for-each>
                </j:array>
                <j:array key="elements">
                    <xsl:for-each select="//tei:elementSpec">
                        <xsl:sort select="@ident"/>
                        <xsl:call-template name="getMember">
                            <xsl:with-param name="attributes" select="true()" />
                        </xsl:call-template>
                    </xsl:for-each>
                </j:array>
                <j:map key="classes">
                    <j:array key="models">
                        <xsl:for-each select="//tei:classSpec[@type='model']">
                            <xsl:sort select="@ident"/>
                            <xsl:call-template name="getMember">
                                <xsl:with-param name="attributes" select="false()" />
                            </xsl:call-template>
                        </xsl:for-each>
                    </j:array>     
                    <j:array key="attributes">
                        <xsl:for-each select="//tei:classSpec[@type='atts']">
                            <xsl:sort select="@ident"/>
                            <xsl:call-template name="getMember">
                                <xsl:with-param name="attributes" select="true()" />
                            </xsl:call-template>
                        </xsl:for-each>
                    </j:array>
                </j:map>                
                <j:array key="elementRefs">
                    <xsl:for-each select="//tei:elementRef[not(ancestor::tei:content)]">
                        <xsl:sort select="@key"/>
                        <j:map>
                            <j:string key="key">
                                <xsl:value-of select="@key"/>
                            </j:string>
                            <xsl:call-template name="desc"/>
                            <xsl:call-template name="mode"/>
                        </j:map>
                    </xsl:for-each>
                </j:array>
                <j:array key="classRefs">
                    <xsl:for-each select="//tei:classRef[not(ancestor::tei:content)]">
                        <xsl:sort select="@key"/>
                        <j:map>
                            <j:string key="key">
                                <xsl:value-of select="@key"/>
                            </j:string>
                            <xsl:call-template name="desc"/>
                            <xsl:call-template name="mode"/>
                        </j:map>
                    </xsl:for-each>
                </j:array>
                <j:array key="macros">
                    <xsl:for-each select="//tei:macroSpec[@type='pe' or not(@type)]">
                        <xsl:sort select="@ident"/>
                        <j:map>
                            <j:string key="ident">
                                <xsl:value-of select="@ident"/>
                            </j:string>
                            <j:string key="module">
                                <xsl:value-of select="@module"/>
                            </j:string>
                            <j:string key="type">
                                <xsl:value-of select="@type"/>
                            </j:string>
                            <xsl:call-template name="desc"/>
                            <xsl:call-template name="mode"/>
                        </j:map>
                    </xsl:for-each>
                </j:array>
                <j:array key="datatypes">
                    <xsl:for-each select="//tei:dataSpec">
                        <xsl:sort select="@ident"/>
                        <xsl:call-template name="getMember">
                            <xsl:with-param name="attributes" select="false()" />
                        </xsl:call-template>
                    </xsl:for-each>
                </j:array>
                <j:array key="macroRefs">
                    <xsl:for-each select="//tei:macroRef[not(ancestor::tei:content)]">
                        <xsl:sort select="@key"/>
                        <j:map>
                            <j:string key="key">
                                <xsl:value-of select="@key"/>
                            </j:string>
                            <xsl:call-template name="desc"/>
                            <xsl:call-template name="mode"/>
                        </j:map>
                    </xsl:for-each>
                </j:array>
            </j:map>
        </xsl:variable>
        <xsl:value-of select="xml-to-json($structure, map{'indent':true()})"/>
    </xsl:template>
    
    <xsl:template name="getMember">
        <xsl:param name="attributes" select="false()"/>
        <j:map>
            <j:string key="ident"><xsl:value-of select="@ident"/></j:string>
            <xsl:variable name="nspace"
                select="(@ns,  ancestor::tei:schemaSpec[1]/@ns)[1]"/>
            <xsl:if test="$nspace">
                <j:string key="ns"><xsl:value-of select="$nspace"/></j:string>
            </xsl:if>
            <j:string key="type"><xsl:value-of select="local-name()"/></j:string>
            <j:string key="module"><xsl:value-of select="@module"/></j:string>
            <xsl:call-template name="desc"/>
            <j:array key="altIdent">
                <xsl:for-each select="tei:altIdent">
                    <j:string><xsl:value-of select="."/></j:string>
                </xsl:for-each>                                
            </j:array>
            <xsl:if test="tei:classes">
                <j:map key="classes">
                    <!-- Organize memberOf references by class type. Use the IDENTS key to look for classSpecs in the current ODD, or look for it in the default source -->
                    <!-- If a classSpec can't be located, mark the reference as 'unknown' (RomaJS will not process it) -->
                    <j:array key="model">
                        <xsl:for-each-group select="tei:classes/tei:memberOf" group-by="tei:getClassType(ancestor::tei:classes, @key)">
                            <xsl:if test="current-grouping-key() = 'model'">
                                <xsl:for-each select="current-group()">
                                    <xsl:if test="not(@mode = 'delete')">
                                        <j:string>
                                            <xsl:value-of select="@key"/>
                                        </j:string>
                                    </xsl:if>
                                </xsl:for-each>
                            </xsl:if>
                        </xsl:for-each-group>
                    </j:array>
                    <j:array key="atts">
                        <xsl:for-each-group select="tei:classes/tei:memberOf" group-by="tei:getClassType(ancestor::tei:classes, @key)">
                            <xsl:if test="current-grouping-key() = 'atts'">
                                <xsl:for-each select="current-group()">
                                    <xsl:if test="not(@mode = 'delete')">
                                       <j:string>
                                           <xsl:value-of select="@key"/>
                                       </j:string>
                                    </xsl:if>
                                </xsl:for-each>
                            </xsl:if>
                        </xsl:for-each-group>
                    </j:array>
                    <j:array key="unknown">
                        <xsl:for-each-group select="tei:classes/tei:memberOf" group-by="tei:getClassType(ancestor::tei:classes, @key)">
                            <xsl:if test="not(current-grouping-key())">
                                <xsl:for-each select="current-group()">
                                    <xsl:if test="not(@mode = 'delete')">
                                        <j:string>
                                            <xsl:value-of select="@key"/>
                                        </j:string>
                                    </xsl:if>
                                </xsl:for-each>
                            </xsl:if>
                        </xsl:for-each-group>
                    </j:array>
                </j:map>
            </xsl:if>
            <xsl:if test="$attributes">
                <xsl:choose>
                    <xsl:when test="self::tei:elementSpec">
                        <xsl:call-template name="attributes">
                            <xsl:with-param name="onElement" select="'true'"/>
                        </xsl:call-template>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:call-template name="attributes"/>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:if> 
            <xsl:if test="tei:content">
                <j:array key="content">
                    <xsl:for-each select="tei:content">
                        <xsl:call-template name="getContent"/>
                    </xsl:for-each>                    
                </j:array>
            </xsl:if>
        </j:map>
    </xsl:template>
    
    <xsl:template name="getContent">
        <xsl:for-each select="*">
            <j:map>
                <xsl:choose>
                    <xsl:when test="self::tei:elementRef or self::tei:macroRef or self::tei:classRef">
                        <j:string key="type"><xsl:value-of select="local-name()"/></j:string>
                        <j:string key="key"><xsl:value-of select="@key"/></j:string>
                    </xsl:when>
                    <xsl:when test="self::tei:sequence or self::tei:alternate">
                        <j:string key="type"><xsl:value-of select="local-name()"/></j:string>
                        <j:string key="minOccurs"><xsl:value-of select="if (@minOccurs) then @minOccurs else 1"/></j:string>
                        <j:string key="maxOccurs"><xsl:value-of select="if (@maxOccurs) then @maxOccurs else 1"/></j:string>
                        <j:array key="content">
                            <xsl:call-template name="getContent"/>
                        </j:array>
                    </xsl:when>
                    <xsl:when test="self::tei:anyElement">
                        <j:string key="type"><xsl:value-of select="local-name()"/></j:string>
                        <j:string key="require"><xsl:value-of select="@require"/></j:string>
                        <j:string key="except"><xsl:value-of select="@except"/></j:string>
                    </xsl:when>
                    <xsl:when test="self::tei:dataRef">
                        <j:string key="type"><xsl:value-of select="local-name()"/></j:string>
                        <xsl:call-template name="getDataRef"/>
                    </xsl:when>
                    <xsl:when test="self::tei:valList">
                        <j:string key="type">
                            <xsl:value-of select="local-name()"/>
                        </j:string>
                        <j:array key="valItem">
                            <xsl:for-each select="tei:valItem">
                                <j:map>
                                    <j:string key="ident">
                                        <xsl:value-of select="@ident"/>
                                    </j:string>
                                </j:map>                                    
                            </xsl:for-each>
                        </j:array>
                    </xsl:when>
                    <xsl:otherwise>
                        <j:string key="type"><xsl:value-of select="local-name()"/></j:string>
                    </xsl:otherwise>
                </xsl:choose>
            </j:map>
        </xsl:for-each>
    </xsl:template>
    
    <xsl:template name="mode">
        <xsl:if test="@mode">
            <j:string key="key">
                <xsl:value-of select="@mode"/>
            </j:string>
        </xsl:if>
    </xsl:template>
    
    <xsl:template name="attributes">
        <xsl:param name="onElement" select="'false'"/>
        <j:array key="attributes">
            <xsl:for-each select=".//tei:attDef">
                <j:map>
                    <j:boolean key="onElement"><xsl:value-of select="$onElement"/></j:boolean>
                    <j:string key="ident">
                        <xsl:value-of select="@ident"/>
                    </j:string>
                    <j:string key="mode">
                        <xsl:value-of select="if (not(@mode)) then 'add' else @mode"/>
                    </j:string>
                    <j:string key="ns">
                        <xsl:value-of select="@ns"/>
                    </j:string>  
                    <j:string key="usage">
                        <xsl:choose>
                            <xsl:when test="@usage">
                                <xsl:value-of select="@usage"/>
                            </xsl:when>
                            <xsl:otherwise>
                                <xsl:text>def</xsl:text>
                            </xsl:otherwise>
                        </xsl:choose>                        
                    </j:string>  
                    <xsl:call-template name="desc"/>
                    <j:array key="altIdent">
                        <xsl:for-each select="tei:altIdent">
                            <j:string><xsl:value-of select="."/></j:string>
                        </xsl:for-each>                                
                    </j:array>
                    <j:array key="valDesc">
                        <xsl:for-each select="tei:valDesc">
                            <xsl:choose>
                                <xsl:when test="@xml:lang and @xml:lang = $doclang">
                                    <xsl:call-template name="makeDesc"/>                  
                                </xsl:when>
                                <xsl:when test="@xml:lang and not(@xml:lang = $doclang)"/>
                                <xsl:otherwise>
                                    <xsl:call-template name="makeDesc"/>
                                </xsl:otherwise>
                            </xsl:choose>              
                        </xsl:for-each>
                    </j:array>
                    <j:map key="datatype">
                        <xsl:for-each select="tei:datatype">
                            <j:string key="min">
                                <xsl:choose>
                                    <xsl:when test="@minOccurs">
                                        <xsl:value-of select="@minOccurs"/>
                                    </xsl:when>
                                    <xsl:otherwise>1</xsl:otherwise>
                                </xsl:choose>
                            </j:string>
                            <j:string key="max">
                                <xsl:choose>
                                    <xsl:when test="@maxOccurs">
                                        <xsl:value-of select="@maxOccurs"/>
                                    </xsl:when>
                                    <xsl:otherwise>1</xsl:otherwise>
                                </xsl:choose>
                            </j:string>                            
                            <xsl:for-each select="tei:dataRef">
                                <j:map key="dataRef">
                                    <xsl:call-template name="getDataRef"/>
                                </j:map>
                            </xsl:for-each>
                        </xsl:for-each>
                    </j:map>
                    <xsl:if test="tei:valList">
                        <j:map key="valList">
                            <j:string key="type">
                                <xsl:value-of select="tei:valList/@type"/>
                            </j:string>
                            <j:array key="valItem">
                                <xsl:for-each select="tei:valList/tei:valItem">
                                    <j:map>
                                        <j:string key="ident">
                                            <xsl:value-of select="@ident"/>
                                        </j:string>
                                        <xsl:call-template name="desc"/>
                                        <j:array key="altIdent">
                                            <xsl:for-each select="tei:altIdent">
                                                <j:string><xsl:value-of select="."/></j:string>
                                            </xsl:for-each>                                
                                        </j:array>
                                    </j:map>                                    
                                </xsl:for-each>
                            </j:array>
                        </j:map>                                            
                    </xsl:if>
                </j:map>
            </xsl:for-each>
        </j:array>
    </xsl:template>
    
    <xsl:template name="getDataRef">
        <xsl:if test="@key">
            <j:string key="key"><xsl:value-of select="@key"/></j:string>
        </xsl:if>
        <xsl:if test="@name">
            <j:string key="name"><xsl:value-of select="@name"/></j:string>
        </xsl:if>
        <xsl:if test="@ref">
            <j:string key="ref"><xsl:value-of select="@ref"/></j:string>
        </xsl:if>
        <xsl:if test="@restriction">
            <j:string key="restriction"><xsl:value-of select="@restriction"/></j:string>
        </xsl:if>
        <xsl:if test="tei:dataFacet">
            <j:array key="dataFacet">
                <xsl:for-each select="tei:dataFacet">
                    <j:map>
                        <j:string key="name"><xsl:value-of select="@name"/></j:string>
                        <j:string key="value"><xsl:value-of select="@value"/></j:string>
                    </j:map>
                </xsl:for-each>
            </j:array>
        </xsl:if>
    </xsl:template>
    
    <xsl:template name="serializeElement">
        <xsl:variable name="simplified">
            <xsl:copy-of copy-namespaces="no" select="."/>
        </xsl:variable>
        <j:string><xsl:value-of select="serialize($simplified)"/></j:string>
    </xsl:template>
    
    <xsl:template name="makeDesc">
        <xsl:choose>
            <xsl:when test="$serializeDocs">
                <xsl:call-template name="serializeElement"/>
            </xsl:when>
            <xsl:otherwise>
                <j:string><xsl:sequence select="tei:makeDescription(parent::*,false(),false())"/></j:string>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
    <xsl:template name="desc">
        <j:array key="desc">
            <xsl:for-each select="tei:desc">
                <xsl:choose>
                    <xsl:when test="@xml:lang and @xml:lang = $doclang">
                        <xsl:call-template name="makeDesc"/>                  
                    </xsl:when>
                    <xsl:when test="@xml:lang and not(@xml:lang = $doclang)"/>
                    <xsl:when test="not(@xml:lang)">
                        <xsl:call-template name="makeDesc"/>
                    </xsl:when>
                    <xsl:otherwise/>
                </xsl:choose>                
            </xsl:for-each>
        </j:array>
        <!-- Format the first desc into shortDesc -->
        <j:string key="shortDesc"><xsl:sequence select="tei:makeDescription(.,false(),false())"/></j:string>
        <xsl:if test="$serializeDocs">
            <j:array key="gloss">
                <xsl:for-each select="tei:gloss">
                    <xsl:choose>
                        <xsl:when test="@xml:lang and @xml:lang = $doclang">
                            <xsl:call-template name="serializeElement"/>
                        </xsl:when>
                        <xsl:when test="@xml:lang and not(@xml:lang = $doclang)"/>
                        <xsl:when test="not(@xml:lang)">
                            <xsl:call-template name="serializeElement"/>
                        </xsl:when>
                        <xsl:otherwise/>
                    </xsl:choose>                
                </xsl:for-each>
            </j:array>
        </xsl:if>        
    </xsl:template>
    
</xsl:stylesheet>
