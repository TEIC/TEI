declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace rng="http://relaxng.org/ns/structure/1.0";
declare namespace request="http://exist-db.org/xquery/request";
<List>
{
let $module := request:request-parameter("module", "")
for $c in collection("/db/TEI")//(tei:elementSpec|tei:classSpec|tei:macroSpec)[@module=$module]
return $c
}
</List>
