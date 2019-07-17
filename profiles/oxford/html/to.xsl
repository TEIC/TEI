<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:html="http://www.w3.org/1999/xhtml"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    exclude-result-prefixes="tei html xs"
    version="2.0">
    <!-- import base conversion style -->

    <xsl:import href="../../../html/html.xsl"/>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>

         <p> This library is free software; you can redistribute it and/or
      modify it under the terms of the GNU Lesser General Public License as
      published by the Free Software Foundation; either version 2.1 of the
      License, or (at your option) any later version. This library is
      distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
      without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
      PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
      details. You should have received a copy of the GNU Lesser General Public
      License along with this library; if not, write to the Free Software
      Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA </p>
         <p>Author: See AUTHORS</p>
         <p>Id: $Id: to.xsl 12482 2013-07-28 18:39:41Z louburnard $</p>
         <p>Copyright: 2013, TEI Consortium</p>
      </desc>
   </doc>

   <xsl:output method="xhtml" omit-xml-declaration="yes"/>

   <xsl:key name="TABLESORT" use="1" match="tei:table[tei:match(@rend,'sort')]"/>

   <xsl:param name="institution">University of Oxford</xsl:param>

    <xsl:param name="treestyle">d3CollapsableTree</xsl:param>

   <xsl:template name="javascriptHook">
     <style type="text/css">
       td {vertical-align: top;}
     </style>
     <xsl:if test="key('TABLESORT',1)">    
       <!-- DataTables CSS -->
       <link rel="stylesheet" type="text/css" href="http://ajax.aspnetcdn.com/ajax/jquery.dataTables/1.9.4/css/jquery.dataTables.css"/>
       
       <!-- jQuery -->
       <script type="text/javascript" charset="utf8" src="http://ajax.aspnetcdn.com/ajax/jQuery/jquery-1.8.2.min.js"><!-- brk --></script>
       
       <!-- DataTables -->
       <script type="text/javascript" charset="utf8"
	       src="http://ajax.aspnetcdn.com/ajax/jquery.dataTables/1.9.4/jquery.dataTables.min.js"><!-- brk --></script>
       
       <script type="text/javascript">
	 $(document).ready(function() {
	 $('table.sort').dataTable( {
	 "sPaginationType": "full_numbers",
	 "bPaginate": true,
	 "bLengthChange": true,
	 "bAutoWidth": false,
	 "bFilter": true,
	 "bSort": true,
	 "bInfo": true,
	 "aaSorting": [ ],
	 "bScrollCollapse": true,
	 "bJQueryUI": true,
	 "sDom": 'flprtip'})});
       </script>
     </xsl:if>
  </xsl:template>
</xsl:stylesheet>
