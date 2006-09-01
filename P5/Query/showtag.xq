declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace rng="http://relaxng.org/ns/structure/1.0";
declare namespace html="http://www.w3.org/1999/xhtml";
declare namespace request="http://exist-db.org/xquery/request";
<TEI xmlns="http://www.tei-c.org/ns/1.0" 
     xmlns:xi="http://www.w3.org/2001/XInclude"> 
{
let $name := request:get-parameter("name", "")
for $c in collection("/db/TEI")//tei:elementSpec[@ident=$name]
return $c
}
</TEI> 