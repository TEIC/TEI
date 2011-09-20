<?xml version="1.0" encoding="UTF-8"?>
<xsl:transform version="2.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:strip-space elements="*"/>
<xsl:output method="html"/> 

<xsl:variable name="summary">
  <xsl:variable name="t-total" select="/trace/end/@time - /trace/@time"/>
  <stats t-total="{$t-total}">
  <xsl:variable name="data">
    <xsl:for-each-group select="//(template|function)" group-by="concat(@file,@line,@name,@match)">
                <xsl:sort select="@file"/>
                <xsl:sort select="@line"/>
                <xsl:sort select="@name"/>
                <xsl:sort select="@match"/>
            <fn>
            <xsl:copy-of select="@* except @time"/>
            <xsl:attribute name="construct" select="name()"/>
            <xsl:variable name="count" select="count(current-group())"/>
            
            <xsl:variable name="group">
                <xsl:for-each select="current-group()">
                    <entry>
                    <xsl:copy-of select="@*"/>
                    <xsl:variable name="gross" select="end/@time - @time"/>
                    <xsl:attribute name="t" select="$gross"/>
                    <xsl:attribute name="t2" select="$gross * $gross"/>
                    <xsl:attribute name="net" 
                                  select="max(($gross - sum(for $i in (function|template)
                                                       return ($i/end/@time - $i/@time)),0))"/>
                    </entry>
                </xsl:for-each>
            </xsl:variable>
            
            <xsl:variable name="t-sum" select="sum($group/entry/@t)"/>
            <xsl:variable name="t-min" select="min($group/entry/@t)"/>
            <xsl:variable name="t-max" select="max($group/entry/@t)"/>
            <xsl:variable name="t-avg" select="avg($group/entry/@t)"/>
            
 <!--           <xsl:variable name="t-M2" select="sum($group/entry/@t2) div $count"/>
            
            <xsl:variable name="log10" xmlns:m="java:java.lang.Math" select="m:log(10)"/>
            <xsl:attribute name="score" xmlns:m="java:java.lang.Math"
                    select="m:log($t-sum * $count) div $log10"/>         -->
            <xsl:attribute name="count" select="$count"/>
            <xsl:attribute name="t-sum" select="$t-sum"/>
            <xsl:attribute name="t-min" select="$t-min"/>
            <xsl:attribute name="t-max" select="$t-max"/>
            <xsl:attribute name="t-avg" select="$t-avg"/>
            <xsl:attribute name="t-sum-net" select="sum($group/entry/@net)"/>
            <xsl:attribute name="t-avg-net" select="avg($group/entry/@net)"/>
 <!--           <xsl:attribute name="t-std" xmlns:m="java:java.lang.Math" select="m:sqrt($t-M2 - ($t-avg * $t-avg))"/>  -->
            </fn>
    </xsl:for-each-group>
  </xsl:variable>

  <xsl:perform-sort select="$data/fn">
    <xsl:sort select="number(@t-sum-net)" order="descending"/>
  </xsl:perform-sort>
  </stats>
</xsl:variable>

<xsl:param name="output" select="'html'"/>

<!-- entry point to get the analyzed XML data out -->

<xsl:template match="/trace[$output='xml']" priority="2">
  <xsl:copy-of select="$summary"/>
</xsl:template>

<xsl:template match="/trace">
  <xsl:apply-templates select="$summary" mode="report"/>
</xsl:template>

<xsl:template match="stats" mode="report">
<html>
  <head><title>Analysis of Stylesheet Execution Time</title></head>
  <body><h1>Analysis of Stylesheet Execution Time</h1>
  <p>Total time: <xsl:value-of select="@t-total"/> milliseconds</p>
  <h2>Time spent in each template or function:</h2>
  <p>The table below is ordered by the total net time spent in the template or function. 
  Gross time means the time including called templates and functions; net time means
  time excluding time spent in called templates and functions.</p>
  <table border="border" cellpadding="10">
    <thead>
        <tr>
          <th>file</th>
          <th>line</th>
          <th>instruction</th>
          <th>count</th>
          <th>average time (gross)</th>
          <th>total time (gross)</th>  
          <th>average time (net)</th>
          <th>total time (net)</th>                   
          </tr>
    </thead>
    <tbody>
      <xsl:for-each select="fn">
        <tr>
            <td><xsl:value-of select="@file"/></td>
            <td><xsl:value-of select="@line"/></td>
            <td><xsl:value-of select="@construct, @name, @match"/></td>      
            <td align="right"><xsl:value-of select="@count"/></td>
            <td align="right"><xsl:value-of select="format-number(@t-avg, '#0.000')"/></td> 
            <td align="right"><xsl:value-of select="format-number(@t-sum, '#0.000')"/></td>  
            <td align="right"><xsl:value-of select="format-number(@t-avg-net, '#0.000')"/></td> 
            <td align="right"><xsl:value-of select="format-number(@t-sum-net, '#0.000')"/></td>                          
        </tr>
      </xsl:for-each>
    </tbody>
  </table>
  </body>
  </html>
</xsl:template>          

</xsl:transform>
