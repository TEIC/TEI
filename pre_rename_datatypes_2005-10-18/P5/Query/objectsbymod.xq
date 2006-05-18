declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace rng="http://relaxng.org/ns/structure/1.0";
<List>
{
let $module := request:request-parameter("module", "")
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
{
let $module := request:request-parameter("module", "")
for $c in collection("/db/TEI")//(tei:elementSpec|tei:classSpec)[@module=concat($module,'-decl')]
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
