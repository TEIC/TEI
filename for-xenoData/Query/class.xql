declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace rng="http://relaxng.org/ns/structure/1.0";
declare namespace html="http://www.w3.org/1999/xhtml";
declare namespace request="http://exist-db.org/xquery/request";
let $Page:=
<TEI xmlns="http://www.tei-c.org/ns/1.0" 
     xmlns:xi="http://www.w3.org/2001/XInclude"> 
<tei:teiHeader>
    <fileDesc>
      <titleStmt>
        <title>TEI Query</title>
      </titleStmt>
         <editionStmt>
            <edition>
               <date>{current-dateTime()}</date>
            </edition>
      </editionStmt>
      <publicationStmt>
	  <p/>
      </publicationStmt>
      <sourceDesc>
        <p/>
      </sourceDesc>
    </fileDesc>
</tei:teiHeader>
<text>
<body>
<p>Search again:
<html:form action="class.xql" method="GET">
  <html:input name="name" width="15"/>       	       <html:input type="submit" />

</html:form>
</p>
<p>
{
let $name := request:get-parameter("name", "")
for $c in collection("/db/TEI")//tei:classSpec[@ident=$name]
return $c
}
</p>
</body>
</text>
</TEI> 
return  transform:transform($Page, "http://www.tei-c.org/release/xml/tei/stylesheet/teic/odd2html.xsl",<parameters/>)
