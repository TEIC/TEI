declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace rng="http://relaxng.org/ns/structure/1.0";
declare namespace html="http://www.w3.org/1999/xhtml";

<TEI xmlns="http://www.tei-c.org/ns/1.0" 
     xmlns:xi="http://www.w3.org/2001/XInclude"> 
{
let $name := request:request-parameter("name", "")
for $c in collection("/db/TEI")//tei:elementSpec[@ident=$name]|tei:classSpec[@ident=$name]|tei:macroSpec[@ident=$name]
return $c
}
</TEI> 