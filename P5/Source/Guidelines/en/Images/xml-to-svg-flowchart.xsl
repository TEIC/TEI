<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xpath-default-namespace="http://www.tei-c.org/ns/1.0"
    xmlns="http://www.w3.org/2000/svg"
    xmlns:ebb="https://ebb.newtfire.org"
    exclude-result-prefixes="xs ebb"
    version="3.0">
    <!--2019-12-26 ebb: This XSLT creates a flowchart of an XML source document to show its hierarchy, and it was designed to display for a tree hierarchy of four layers deep.  -->
    <xsl:output method="xml" indent="yes"/> 
    <!--2019-12-20 ebb: See making arrowheads in SVG: http://thenewcode.com/1068/Making-Arrows-in-SVG  --> 
    <xsl:variable name="allCount" as="xs:integer" select="count(//*)"/>  
    <xsl:variable as="xs:integer" name="labelSize" select="50"/>
    <xsl:variable as="xs:integer" name="ySpacer" select="30"/>
    <xsl:variable as="xs:integer" name="totalHeight" select="$ySpacer * $allCount"/>
    <xsl:variable as="xs:integer" name="label-length" select="100"/>
    <xsl:variable as="xs:integer" name="xSpacer" select="300"/>
    <xsl:variable as="xs:integer" name="horizontalLine" select="25"/>
   <!-- <xsl:variable name="numberOfLevels" as="xs:integer" select="//*/ancestor::*/name() => distinct-values() => count()"/>--><!--ebb: This way of calculating the number of levels in the hierarchy seems a little wrong to me, but a count of all the ancestors generates too many. -->
    <xsl:variable name="numberOfLevels" as="xs:integer" select="(//*)[last()]/ancestor::* => count()"/>
    <xsl:variable name="plotWidth" as="xs:integer" select="$numberOfLevels*$xSpacer + $label-length + $horizontalLine * 2 + 20 "/>
    <xsl:variable name="ChildCountPerLevel" as="xs:integer+">
        <xsl:for-each select="//*/ancestor::*">
            <xsl:value-of select="count(child::*)"/>
        </xsl:for-each>
    </xsl:variable>
    <xsl:variable name="maxChildCountPerLevel" as="xs:integer" select="max($ChildCountPerLevel)"/>
    <xsl:variable name="plotHeight" as="xs:integer" select="$maxChildCountPerLevel * $ySpacer + 200"/>
 
    <xsl:function name="ebb:flowchart">
        <xsl:param name="input" as="element()+"/>
        <!--2019-12-21 ebb: What's to do is count the immediate children of the current node, and divide that by the $lineCount (the total number of lines that helps us plot the max Y space in the plot, and space out based on position. Find out what level in the tree they are by counting ancestor elements. Make SVG elements and spread them out based on how many there are, and what level they are in the XML hierarchy. Then continue applying this function to each child of the input child. This needs to recurse through the tree. -->
        <xsl:variable name="countChildren" as="xs:integer" select="$input/child::* => count()"/>
        <xsl:variable name="countFollowing" as="xs:integer" select="$input/following::* => count()"/>
        <xsl:variable name="level" as="xs:integer" select="$input/ancestor::* => count()"/>
        <g class="level-{$level}">
            <xsl:for-each select="$input/child::*">         
                <xsl:variable name="nestingSpacer" as="xs:float" select="(count((descendant::*, preceding::*)) - count(following-sibling::*[child::*])) *$ySpacer"/>
                <xsl:variable name="yPos" as="xs:float" select="-$totalHeight + count(preceding::*[not(child::*)]) * $ySpacer + $nestingSpacer"/>
                <xsl:variable name="next-nestingSpacer-Up" select=" (count((child::*[1]/descendant::*, child::*[1]/preceding::*)) - count(child::*[1]/following-sibling::*[child::*])) * $ySpacer"/> 
                <xsl:variable name="next-nestingSpacer-Down" select="count((child::*[last()]/descendant::*, child::*[last()]/preceding::*)) * $ySpacer"/>  <!--ebb: There aren't any following siblings after the last() child, so we don't need to subtract to calculate the next-nestingSpacer-Down. -->
                
                <xsl:variable name="next-yPos-Down" as="xs:float" select="-$totalHeight + count(child::*[last()]/preceding::*[not(child::*)])  * $ySpacer + $next-nestingSpacer-Down"/>
                <xsl:variable name="next-yPos-Up" as="xs:float" select="-$totalHeight + count(child::*[1]/preceding::*[not(child::*)]) * $ySpacer + $next-nestingSpacer-Up"/>
                
                <line x1="{($level) * $xSpacer }" x2="{($level) * $xSpacer + $label-length}" y1="{$yPos}" y2="{$yPos}" stroke="lightsteelblue" stroke-linecap="round" stroke-width="{$labelSize}"/>  
                <text x="{($level) * $xSpacer + $label-length div 2}" y="{$yPos + 5}" text-anchor="middle" font-size="22" font-weight="bold" fill="black"><xsl:value-of select="name()"/></text> 
                <!--Horizontal line with arrow coming from previous level -->
                <line x1="{($level - 1) * $xSpacer + $label-length + (2*$horizontalLine) + 20}" x2="{($level) * $xSpacer - 45}" y1="{$yPos}" y2="{$yPos}" stroke="#70a2e3" stroke-width="2" marker-end="url(#arrowhead)" />
                
                <xsl:if test="child::*">  
                    
                    <!--Horizontal line with arrow pointing to next level -->
                    <line x1="{($level) * $xSpacer + $label-length + $horizontalLine}" x2="{($level) * $xSpacer + $label-length + (2*$horizontalLine)}" y1="{$yPos}" y2="{$yPos}" stroke="#70a2e3" stroke-width="2" marker-end="url(#arrowhead)" />
                    <!--Vertical line spanning next level -->
                    <line x1="{($level) * $xSpacer + $label-length + (2*$horizontalLine) + 20}" x2="{($level) * $xSpacer + $label-length + (2*$horizontalLine) + 20}" y1="{$next-yPos-Up}" y2="{$next-yPos-Down}" stroke="#70a2e3" stroke-width="2"/>
                  
                    <xsl:sequence select="ebb:flowchart(.)"/>                  
                </xsl:if>         
                              
            </xsl:for-each>
        </g> 
        
    </xsl:function>
    
    <xsl:template match="/">
        <svg width="100%" height="100%" viewBox="0 0 {$plotWidth} {$totalHeight * 2}">
            <defs>
                <marker id="arrowhead" markerWidth="10" markerHeight="7" 
                    refX="0" refY="3.5" orient="auto">
                    <polygon points="0 0, 10 3.5, 0 7" />
                </marker>
            </defs>
            <g id="root" transform="translate({400}, {$totalHeight + 200})">  
                <xsl:comment>Total number of levels is: <xsl:value-of select="$numberOfLevels"/>.
                The maximum child count for any level is: <xsl:value-of select="$maxChildCountPerLevel"/>.
                </xsl:comment>
                <xsl:variable name="next-nestingSpacer-Up" select=" (count((/*/*[1]/descendant::*, child::*[1]/preceding::*)) - count(/*/*[1]/following-sibling::*[child::*])) * $ySpacer"/> 
                <xsl:variable name="next-nestingSpacer-Down" select="count((/*/*[last()]/descendant::*, /*/*[last()]/preceding::*)) * $ySpacer"/>  <!--ebb: There aren't any following siblings after the last() child, so we don't need to subtract to calculate the next-nestingSpacer-Down. -->
                
                <xsl:variable name="next-yPos-Down" as="xs:float" select="-$totalHeight + count(/*/*[last()]/preceding::*[not(child::*)])  * $ySpacer + $next-nestingSpacer-Down"/>
                <xsl:variable name="next-yPos-Up" as="xs:float" select="-$totalHeight + count(/*/*[1]/preceding::*[not(child::*)]) * $ySpacer + $next-nestingSpacer-Up"/>   
                <xsl:variable name="yPos" as="xs:float" select=" ($next-yPos-Up + $next-yPos-Down) div 2"/>
                <!--ebb: This yPos is designed to position the root element SVG shapes in between the positioning of its two children. 
              My earlier version of this yPos was a bit ad hoc: 
              -$totalHeight + count(descendant::*) * $ySpacer div 1.25 -->
                                
                <line x1="-300" y1="{$yPos}" x2="{-300 + $label-length}" y2="{$yPos}" stroke="lightsteelblue" stroke-linecap="round" stroke-width="{$labelSize}"/>
                <text x="{-300 + $label-length div 2}" y="{$yPos + 5}" text-anchor="middle" font-size="22" font-weight="bold" fill="black"><xsl:value-of select="/*/name()"/></text>
                <line x1="{-300 + $label-length + 25}" y1="{$yPos}" x2="{-300 + $label-length + 50}" y2="{$yPos}" stroke="#70a2e3" stroke-width="2" marker-end="url(#arrowhead)" />
                <!--Horizontal line with arrow pointing to next level -->
                <line x1="{-300 + $label-length + $horizontalLine}" x2="{-300 + $label-length + (2*$horizontalLine)}" y1="{$yPos}" y2="{$yPos}" stroke="#70a2e3" stroke-width="2" marker-end="url(#arrowhead)" />
                <!--Vertical line spanning next level -->
                <line x1="{-300 + $label-length + (2*$horizontalLine) + 20}" x2="{-300 + $label-length + (2*$horizontalLine) + 20}" y1="{$next-yPos-Up}" y2="{$next-yPos-Down}" stroke="#70a2e3" stroke-width="2"/>
                
                <xsl:sequence select="ebb:flowchart(./*)"/>
                
            </g>
        </svg>  
        
    </xsl:template>
    
    
</xsl:stylesheet>