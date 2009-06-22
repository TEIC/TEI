declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace rng="http://relaxng.org/ns/structure/1.0";
declare namespace request="http://exist-db.org/xquery/request";
<list>
{
let $class := request:get-parameter("class", "")
for $e in
collection("/db/TEI")//(tei:elementSpec|tei:classSpec)[tei:classes/tei:memberOf[@key=$class]]
order by $e/@ident
	return
	<item>{data($e/@ident)}</item>
}

</list>
