declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace rng="http://relaxng.org/ns/structure/1.0";
declare namespace request="http://exist-db.org/xquery/request";
let $name := request:get-parameter("name", "")
let $lang := request:get-parameter("lang", "en")
for $c in collection("/db/TEI")//tei:elementSpec[@ident=$name]
let $Desc:=
    if ($c/tei:desc[@xml:lang=$lang]) then
        $c/tei:desc[@xml:lang=$lang]
    else
        $c/tei:desc[not(@xml:lang)]
return
<Element>
  <elementName>{data($c/@ident)}</elementName>
  <elementDesc>{data($Desc)}</elementDesc>
  <elementContent>{$c/tei:content/*}</elementContent>
  <elementClasses>
  {
for $class in $c/tei:classes/tei:memberOf
return
    <class>{data($class/@key)}</class>
  }
  </elementClasses>
</Element>
