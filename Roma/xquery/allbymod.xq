declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace rng="http://relaxng.org/ns/structure/1.0";
<List>
{
let $module := request:request-parameter("module", "")
for $c in collection("/db/TEI")//(tei:elementSpec|tei:classSpec)[@module=$module]
return $c
}
{
let $module := request:request-parameter("module", "")
for $c in collection("/db/TEI")//(tei:elementSpec|tei:classSpec)[@module=concat($module,'-decl')]
return $c
}
</List>
