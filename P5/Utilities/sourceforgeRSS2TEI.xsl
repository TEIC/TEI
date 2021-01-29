<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns="http://www.tei-c.org/ns/1.0" 
		xmlns:content="http://purl.org/rss/1.0/modules/content/" 
		>
<!--
On the suspicion that I wasn't the only one has found tracking down 
tickets in the last two days a bit of a pain, I have written a script 
that converts the RSS feeds from sourceforge into TEI.

Current known issues:

* Double escaping of &lt;, > and &amp; (that's how it comes in the RSS and | 
didn't want to start shaving that yak)
* Limit of 100 bugs + 100 features (can use multiple requests + xinclude 
to get more if needed?)
* Comments appear unavailable via API (and i'm not scraping the HTML)
* title field may be truncated if it contains a &lt;
* Dates aren't in an easily sortable format
* The header is impoverished, as is usual for such documents

How I use the script:

wget -O bugs.rss "http://sourceforge.net/api/artifact/index/tracker-id/644062/limit/1000/rss"
wget -O features.rss  "http://sourceforge.net/api/artifact/index/tracker-id/644065/limit/1000/rss"
xsltproc ./sourceforgeRSS2TEI.xsl ./bugs.rss > bugs.xml
xsltproc ./sourceforgeRSS2TEI.xsl ./features.rss > features.xml

Stuart Yeates
Library Technology Services http://www.victoria.ac.nz/library/
-->

  <xsl:output method="xml"
	      encoding="ASCII"/>

  <xsl:template match="rss">
    <TEI>
      <teiHeader>
        <fileDesc>
          <titleStmt>
            <title><xsl:value-of select="channel/title"/></title>
          </titleStmt>
          <publicationStmt>
            <p>Automatically generated content. Do ont edit unless you are confident it is not going to be overwritten.</p>
          </publicationStmt>
          <sourceDesc>
            <p>Transmogrified from the sourceforge RSS feed.</p>
          </sourceDesc>
        </fileDesc>
      </teiHeader>
      <text>
        <div>
          <table>
            <head>TEI tickets</head>
            <row>
              <cell type="submitterRealName">Submitter Real Name </cell>
              <cell type="assignedToRealName">Assigned to Real Name </cell>
              <cell type="ticketStatus"> Status </cell>
              <cell type="ticketPriority"> Priority </cell>
              <cell type="ticketCategory"> Category </cell>
              <cell type="submitterUserName">Submitter Username </cell>
              <cell role="label" type="ticketID">ID </cell>
              <cell type="submitterEmail">Submitter email address</cell>
              <cell type="submitterHomePage">Submitter home page</cell>
              <cell type="ticketLink">Link to ticket</cell>
              <cell type="ticketComments">Link to comments</cell>
              <cell role="label" type="ticketTitle">Title </cell>
              <cell type="ticketPublicationDate"> Date</cell>
              <cell type="ticketLongDescription"> Description </cell>     
            </row> 

            <xsl:apply-templates/>
          </table>
        </div>
      </text> 
    </TEI> 
  </xsl:template>

  
  <xsl:template match="item">
    <row>
      <cell type="submitterRealName"><xsl:value-of select="substring-before(substring-after(content:encoded, 'Submitted By: '), '&lt;')"/> </cell>
      <cell type="assignedToRealName"><xsl:value-of select="substring-before(substring-after(content:encoded, 'Assigned To: '), '&lt;')"/> </cell>
      <cell type="ticketStatus"><xsl:value-of select="substring-before(substring-after(content:encoded, 'Status: '), '&lt;')"/> </cell>
      <cell type="ticketPriority"><xsl:value-of select="substring-before(substring-after(content:encoded, 'Priority: '), '&lt;')"/> </cell>
      <cell type="ticketCategory"><xsl:value-of select="substring-before(substring-after(content:encoded, 'Category: '), '&lt;')"/> </cell>
      <cell type="submitterUserName"><xsl:value-of select="substring-before(substring-after(content:encoded, 'Submitted By: '), '&lt;')"/> </cell>
      <cell role="label" type="ticketID"><xsl:value-of select="substring-before(title, ' ')"/> </cell>
      <cell type="submitterEmail"><ptr target="{concat('mailto:',author)}"/></cell>
      <cell type="submitterHomePage"><ptr target="{concat('https://sourceforge.net/users/',substring-before(author, '@'))}"/></cell>
      <cell type="ticketLink"><ptr target="{link}"/></cell>
      <cell type="ticketComments"><ptr target="{comments}"/></cell>
      <cell role="label" type="ticketTitle"><xsl:value-of select="description"/> </cell>
      <cell type="ticketPublicationDate"><date><xsl:value-of select="pubDate"/> </date></cell>
      <cell type="ticketLongDescription"><xsl:value-of select="content:encoded"/> </cell>     
    </row> 
  </xsl:template>

  <!-- eat all random text -->
  <xsl:template match="text()" priority="-1"/>



</xsl:stylesheet>
