<xsl:stylesheet 
    version="2.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:teix="http://www.tei-c.org/ns/Examples"
    xpath-default-namespace="http://www.tei-c.org/ns/1.0">
  
  <xsl:output method="html" indent="yes" encoding="utf-8"/>
  
  <xsl:variable name="doc" select="/"/>
  
  <xsl:template match="/">
    <html>
      <head>
	<script type="text/javascript">
<![CDATA[
/*
Library for creating a sortable table. Depends on there being thead/td and tbody in the table.

This needs to find any table which has a "sortable" class, and make the headers
clickable to sort the table. 

Martin Holmes September 2011. TO BE COMPLETED.
*/

//Add an event listener to the onload event.
if (window.addEventListener) window.addEventListener('load', makeTablesSortable, false)
else if (window.attachEvent) window.attachEvent('onload', makeTablesSortable);

//The array used to stash table rows before
//sorting them.
rows = new Array();

//A class for containing a row and associated sort key.
function RowObj(){
  this.row = null;
  this.sortKey = '';
}

//Up and down pointing triangles used for showing direction of sort.
//var upTriangle = '&#x25b2;';
//var downTriangle = '&#x25bc;';
var upTriangle = '\u25b2';
var downTriangle = '\u25bc';

//Utility function.
String.prototype.trim = function () {
    return this.replace(/^\s*/, "").replace(/\s*$/, "");
}

function makeTablesSortable(){
//Find each table with the correct class attribute and make them sortable.
  var tables = document.getElementsByTagName('table');
  for (var i=0; i<tables.length; i++){
    if (tables[i].className.search(/\bsortable\b/) != -1) {
        makeSortable(tables[i]);
    }
  }
}

function makeSortable(table){
  var headers = table.getElementsByTagName('th');
  if (headers.length < 1){
    var theads = table.getElementsByTagName('thead');
    if (theads.length > 0){
      headers = theads[0].getElementsByTagName('td');
    }
  }
  for (var i=0; i<headers.length; i++){
    headers[i].style.cursor = 'pointer';
    headers[i].setAttribute('onclick', 'sortByCol(this, ' + i + ')');
    headers[i].setAttribute('title', 'Click to sort the table by this column.');
  }
}

function sortByCol(sender, sortCol){
//Clear the existing array.
  rows = [];
  
//Get a pointer to the ancestor table element.
  var table = sender;
  while (table.tagName.toLowerCase() != 'table'){
    table = table.parentNode;
  }

//Find its tbody. Assume there's only one.
  var tbodies = table.getElementsByTagName('tbody')
  if (tbodies.length <1) return;
  
  tbody = tbodies[0];
  
//This is used to check what kind of data we're sorting on.
  var textSort = false;
  
//Detach all the rows.
  var tbodyRows = tbody.getElementsByTagName('tr');
  
  for (var i=tbodyRows.length-1; i>=0; i--){
    var rowCells = tbodyRows[i].getElementsByTagName('td');

    var sortKey = '';
    if (rowCells.length > sortCol){
      sortKey = rowCells[sortCol].textContent.toLowerCase().trim();
      
      if (sortKey.length > 0){
        textSort = (isNaN(sortKey) || textSort);
      }
    }
    rowObj = new RowObj();
    rowObj.row = tbodyRows[i].parentNode.removeChild(tbodyRows[i]); 
    rowObj.sortKey = sortKey;
    rows.push(rowObj);
  }

var sortAsc = true;

//Now figure out which direction to sort in.
var triangle = document.getElementById('sortTriangle');
if (triangle != null){
  if (triangle.parentNode == sender){
    if (triangle.textContent == downTriangle){
      sortAsc = false;
      triangle.textContent = upTriangle;
    }
    else{
      triangle.textContent = downTriangle;
    }
  }
  else{
    sender.appendChild(triangle.parentNode.removeChild(triangle));
    triangle.textContent = downTriangle;
  }
}
else{
  triangle = document.createElement('span');
  triangle.style.paddingLeft = '0.5em';
  triangle.setAttribute('id', 'sortTriangle');
  triangle.textContent = downTriangle;
  sender.appendChild(triangle);
}

//Sort them.
  if (textSort == true){
    sortAsc ? rows.sort(textSortAsc) : rows.sort(textSortDesc);
  }
  else{
    sortAsc ? rows.sort(numSortAsc) : rows.sort(numSortDesc);
  }

//Re-attach them to tbody.
  for (var i=0; i<rows.length; i++){
    tbody.appendChild(rows[i].row);
  }
}

//Comparator functions
function textSortAsc(a, b){
//alert(a.sortKey + ' : ' + b.sortKey + ' = ' + ((a.sortKey < b.sortKey) ? -1: ((a.sortKey > b.sortKey) ? 1 : 0)));
  return ((a.sortKey < b.sortKey) ? -1: ((a.sortKey > b.sortKey) ? 1 : 0));
}
function textSortDesc(a, b){
  return ((b.sortKey < a.sortKey) ? -1: ((b.sortKey > a.sortKey) ? 1 : 0));
}
function numSortAsc(a, b){
  return (parseFloat(a.sortKey) - parseFloat(b.sortKey));
}
function numSortDesc(a, b){
  return (parseFloat(b.sortKey) - parseFloat(a.sortKey)); 
}
]]></script>
</head>
      <body>
	<xsl:variable name="list">n place rend role status type xml:lang</xsl:variable>
	<ul>
	  <xsl:for-each select="tokenize($list,' ')">
	    <li><a href="#{.}"><xsl:value-of select="."/></a></li>
	  </xsl:for-each>
	</ul>
	
	<xsl:for-each select="tokenize($list,' ')">
	  <xsl:call-template name="list">
	    <xsl:with-param name="name"><xsl:value-of select="."/></xsl:with-param>
	  </xsl:call-template>
	</xsl:for-each>
      </body>
    </html>
  </xsl:template>

<xsl:template name="list">
  <xsl:param name="name"/>
  <h1 id="{$name}">Attribute <xsl:value-of select="$name"/></h1>
  <xsl:for-each select="$doc">
    <xsl:variable name="contents">
      <xsl:for-each select="//teix:*/@*[name()=$name]">
	<tei:foo>
	  <xsl:value-of select="."/>
	  <xsl:text>&gt;</xsl:text>
	  <xsl:value-of select="name(..)"/> 
      </tei:foo> 
      </xsl:for-each>
    </xsl:variable>
    <table class="sortable">
      <thead>
	<tr>
	<th>Value</th>  
	<th>Parent element</th>  
	<th>Occurrences</th>  
	</tr>
      </thead>
    <xsl:for-each-group select="$contents/foo" group-by=".">
      <xsl:sort select="."/>
      <tr>
	<xsl:for-each select="tokenize(current-grouping-key(),'&gt;')">
	  <td><xsl:value-of select="."/></td>
	</xsl:for-each>
	<td><xsl:value-of select="count(current-group())"/></td>
      </tr>
    </xsl:for-each-group>
    </table>
  </xsl:for-each>
</xsl:template>
</xsl:stylesheet>
