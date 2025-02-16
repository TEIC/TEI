<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:math="http://www.w3.org/2005/xpath-functions/math"
    xmlns:xd="http://www.oxygenxml.com/ns/doc/xsl"
    xmlns:map="http://www.w3.org/2005/xpath-functions/map"
    exclude-result-prefixes="#all"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:teix="http://www.tei-c.org/ns/Examples"
    xpath-default-namespace="http://www.tei-c.org/ns/1.0"
    version="3.0">
    <xd:doc scope="stylesheet">
        <xd:desc>
            <xd:p><xd:b>Created on:</xd:b> Feb 16, 2025</xd:p>
            <xd:p><xd:b>Author:</xd:b> takeda</xd:p>
            <xd:p>Creates a list of all specs by p5 subset 
            (where available).</xd:p>
        </xd:desc>
    </xd:doc>
    
    <xsl:param name="VaultPath" select="'https://tei-c.org/Vault/P5/'"/>
    <xsl:param name="ReleaseNotes" select="'../ReleaseNotes'" as="xs:string"/>
    
    <xd:doc>
        <xd:desc>All of the URIs for the READMEs, which list all P5 versions.
            These may also provide some additional metadata that
            could be useful in future?</xd:desc>
    </xd:doc>
    <xsl:variable name="READMEs"
        select="uri-collection($ReleaseNotes || '?select=*.xml;recurse=no')" as="xs:anyURI+"/>
    
    <xd:doc>
        <xd:desc>Not all all of the READMEs use full semantic versioning, but the
        Releases in the Vault do, so we need to normalize.</xd:desc>
    </xd:doc>
    <xsl:variable name="versions" as="xs:string+">
        <xsl:for-each select="$READMEs">
            <xsl:variable name="version" 
                select="
                substring-after(., 'readme-') 
                => substring-before('.xml')"/>
            <!--Tokenize and then string together the first 3 numbers
                (e.g. so that
                1 => 1.0.0
                1.2 => 1.2.0
                3.4.2 => 3.4.2) -->
            <xsl:variable name="semVer" as="xs:string"
                select="
                (tokenize($version,'\.'), ('0','0'))[position() lt 4]
                => string-join('.')"/>
            <xsl:sequence select="$semVer"/>
        </xsl:for-each>
    </xsl:variable>
    
    <xd:doc>
        <xd:desc>The main map, listing the VERSION of P5 and the idents defined in that
        subset.</xd:desc>
        <xd:return>
            A map that looks something like:
            <xd:pre>
                {
                    "2.1.1": {
                       "Specs": {
                            "forestGrp": "elementSpec",
                            ...
                        }
                    }
                }
            </xd:pre>
        </xd:return>
    </xd:doc>
    <xsl:variable name="version-to-ident-map" as="map(*)">
        <xsl:map>
            <xsl:for-each select="$versions">
                <xsl:sort select="."/>
                <xsl:message select="'Processing ' || ."/>
                <xsl:variable name="p5Subset" 
                    select="tei:getP5Subset(.)" 
                    as="document-node()?"/>
                <xsl:map-entry key=".">
                    <xsl:map>
                        <xsl:map-entry key="'Specs'">
                            <xsl:map>
                                <xsl:apply-templates 
                                    select="$p5Subset" mode="subset"/>
                            </xsl:map>
                        </xsl:map-entry>
                    </xsl:map>
                </xsl:map-entry>
            </xsl:for-each>            
        </xsl:map>
    </xsl:variable>
    
    <xd:doc>
        <xd:desc>The inverse of the version to ident map, 
            which lists the idents to the versions in which 
            that ident is defined.</xd:desc>
        <xd:return>
            A map that has idents as keys and versions as an array:
            <xd:pre>
                {
                  "gender": ["4.5.0",
                             "4.6.0", 
                             "4.7.0",
                             "4.8.0",
                             "4.8.1",
                             "4.9.0"],
                  ...
                }
            </xd:pre>
        </xd:return>
    </xd:doc>
    <xsl:variable name="ident-to-version-map" as="map(*)">
        <xsl:map>
            <!--Retrieve all of the keys from the version to
                ident map (e.g. all of the versions) and then group by
                the sequence of idents defined (as keys) for that version
            -->
            <xsl:for-each-group select="map:keys($version-to-ident-map)"
                group-by="map:keys(map:get(map:get($version-to-ident-map, .), 'Specs'))">
                <xsl:variable name="specIdent"
                    select="current-grouping-key()" 
                    as="xs:string"/>
                <xsl:variable name="versions" 
                    select="current-group()" 
                    as="xs:string+"/>
                <xsl:map-entry key="$specIdent" 
                    select="array{sort($versions)}"/>
            </xsl:for-each-group>
        </xsl:map>
    </xsl:variable>
    
    <xd:doc>
        <xd:desc>Main driver template. At the moment, just
        produces a map of versions and idents.</xd:desc>
    </xd:doc>
    <xsl:template name="go">
        <xsl:result-document href="version-to-ident.json" method="json">
            <xsl:sequence select="$version-to-ident-map"/>
        </xsl:result-document>
        <xsl:result-document href="ident-to-version.json" method="json">
            <xsl:sequence select="$ident-to-version-map"/>
        </xsl:result-document>
    </xsl:template>
    
    
    <xsl:mode name="subset" on-no-match="shallow-skip"/>
    
    <xd:doc>
        <xd:desc>Process all specs; for now, just put the ident
        as a key with the type of thing as the value.</xd:desc>
    </xd:doc>
    <xsl:template match="elementSpec | dataSpec | classSpec  | macroSpec" mode="subset">
        <xsl:if test="@ident">
            <xsl:map-entry key="string(@ident)" select="local-name()"/>
        </xsl:if>
    </xsl:template>
    
    <xd:doc>
        <xd:desc>Function to retrieve the P5 subset from the vault, if it's available.</xd:desc>
        <xd:param name="version" as="xs:string">The semantic version of P5 to
        investigate.</xd:param>
    </xd:doc>
    <xsl:function name="tei:getP5Subset" as="document-node()?">
        <xsl:param name="version" as="xs:string"/>
        <xsl:variable name="subsetURL"
            as="xs:string"
            select="$VaultPath || $version || '/xml/tei/odd/p5subset.xml'"/>
        <xsl:try>
            <xsl:sequence select="document($subsetURL)"/>
            <xsl:catch>
                <xsl:message>Unable to retrieve <xsl:value-of select="$subsetURL"/></xsl:message>
                <xsl:sequence select="()"/>
            </xsl:catch>
        </xsl:try>
    </xsl:function>
</xsl:stylesheet>