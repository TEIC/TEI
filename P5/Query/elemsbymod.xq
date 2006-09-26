declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace rng="http://relaxng.org/ns/structure/1.0";
declare namespace request="http://exist-db.org/xquery/request";
<elementList>
{
let $module := request:get-parameter("module", "")
let $lang := request:get-parameter("lang", "en")
for $c in collection("/db/TEI")//tei:elementSpec[@module=$module]
let $Desc:=
    if ($c/tei:desc[@xml:lang=$lang]) then
        $c/tei:desc[@xml:lang=$lang]
    else
        $c/tei:desc[not(@xml:lang)]
return
<teiElement>
  <elementName>{data($c/@ident)}</elementName>
  <elementDesc>{data($Desc)}</elementDesc>
  <elementContent>{$c/tei:content/*}</elementContent>
  <elementAttributes>
  {
for $att in $c/tei:attList//tei:attDef
return
    <attribute>{data($att/@ident)}</attribute>
  }
  </elementAttributes>
  <elementClasses>
  {
for $class in $c/tei:classes/tei:memberOf
return
    <class>{data($class/@key)}</class>
  }
  </elementClasses>
</teiElement>
}
</elementList>
