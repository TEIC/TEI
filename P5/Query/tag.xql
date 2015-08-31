declare namespace fn="http://www.w3.org/2003/05/xpath-functions";
declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace rng="http://relaxng.org/ns/structure/1.0";
declare namespace html="http://www.w3.org/1999/xhtml";
declare namespace request="http://exist-db.org/xquery/request";
declare namespace util="http://exist-db.org/xquery/util";

let $Page:=
<tei:TEI xmlns="http://www.tei-c.org/ns/1.0" 
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
<tei:text>
<tei:body>
<tei:p>
{
let $name := request:get-parameter("name", "")
for $c in collection("/db/TEI")//(tei:elementSpec|tei:classSpec|tei:macroSpec)[@ident=$name]
return $c
}
</tei:p>
<tei:p>Search again:
<html:form action="tag.xql?lang={
request:get-parameter("documentationLanguage", "")
}" method="GET">
  <html:input name="name" width="15"/>
<html:input type="submit" />

</html:form>
</tei:p>
</tei:body>
</tei:text>
</tei:TEI> 
return  transform:transform($Page, "http://www.tei-c.org/release/xml/tei/stylesheet/teic/odd2html.xsl",<parameters/>)
       
