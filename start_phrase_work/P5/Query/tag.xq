declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace rng="http://relaxng.org/ns/structure/1.0";
declare namespace html="http://www.w3.org/1999/xhtml";

<TEI xmlns="http://www.tei-c.org/ns/1.0" 
     xmlns:xi="http://www.w3.org/2001/XInclude"> 
<text>
<body>
<p>Search again:
<html:form action="tag.xq" method="GET">
  <html:input name="name" width="15"/>       	       <html:input type="submit" />

</html:form>
</p>
<p>
{
let $ename := request:request-parameter("name", "")
for $c in collection("/db/TEI")//i18n/element[@ident=$ename]
return $c
}
</p>
<p>
{
let $name := request:request-parameter("name", "")
for $c in collection("/db/TEI")//(tei:elementSpec|tei:classSpec|tei:macroSpec)[@ident=$name]
return $c
}
</p>
</body>
</text>
</TEI> 