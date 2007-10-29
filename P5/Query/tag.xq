declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace rng="http://relaxng.org/ns/structure/1.0";
declare namespace html="http://www.w3.org/1999/xhtml";
declare namespace request="http://exist-db.org/xquery/request";
<tei:TEI xmlns="http://www.tei-c.org/ns/1.0" 
     xmlns:xi="http://www.w3.org/2001/XInclude"> 
<tei:text>
<tei:body>
<tei:p>
{
let $ename := request:get-parameter("name", "")
for $c in collection("/db/TEI")//i18n/element[@ident=$ename]
return $c
}
</tei:p>
<tei:p>
{
let $name := request:get-parameter("name", "")
for $c in collection("/db/TEI")//(tei:elementSpec|tei:classSpec|tei:macroSpec)[@ident=$name]
return $c
}
</tei:p>
<tei:p>Search again:
<html:form action="tag.xq?lang={
request:get-parameter("documentationLanguage", "")
}" method="GET">
  <html:input name="name" width="15"/>
<html:input type="submit" />

</html:form>
</tei:p>
</tei:body>
</tei:text>
</tei:TEI> 