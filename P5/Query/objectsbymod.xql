declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace rng="http://relaxng.org/ns/structure/1.0";
declare namespace request="http://exist-db.org/xquery/request";
<List>
{
let $module := request:get-parameter("module", "")
for $c in collection("/db/TEI")//(tei:elementSpec|tei:classSpec)[@module=$module]
return
<Object>
  <Type>{name($c)}</Type>
  <Name>{data($c/@ident)}</Name>
  <Attributes>
  {
for $att in $c/tei:attList//tei:attDef
return
    <attribute>{data($att/@ident)}</attribute>
  }
  </Attributes>
</Object>
}
</List>
