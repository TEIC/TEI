declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace rng="http://relaxng.org/ns/structure/1.0";
<elementList>
{
let $module := request:request-parameter("module", "")
for $c in collection("/db/TEI")//tei:elementSpec[@module=$module]
order by $c/@ident
return
<teiElement>
  <elementName>{data($c/@ident)}</elementName>
  <elementDesc>{data($c/tei:desc)}</elementDesc>
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
