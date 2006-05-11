declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace rng="http://relaxng.org/ns/structure/1.0";
declare namespace request="http://exist-db.org/xquery/request";
let $name := request:request-parameter("name", "")
for $c in collection("/db/TEI")//tei:elementSpec[@ident=$name]
return
<Element>
  <elementName>{data($c/@ident)}</elementName>
  <elementDesc>{data($c/tei:desc)}</elementDesc>
  <elementContent>{$c/tei:content/*}</elementContent>
  <elementClasses>
  {
for $class in $c/tei:classes/tei:memberOf
return
    <class>{data($class/@key)}</class>
  }
  </elementClasses>
</Element>
