declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace request="http://exist-db.org/xquery/request";
let $query := request:get-parameter("query", ""),
$R :=
<OneBoxResults>
<Diagnostics>success</Diagnostics>
<provider>TEI</provider>
<title>
  <urlText>Results from TEI P5</urlText>
  <urlLink>
	http://tei.oucs.ox.ac.uk/Query/search.xq?query={$query}
   </urlLink>
</title>
{for $i in collection("/db/TEI")//(tei:elementSpec|tei:classSpec|tei:macroSpec)[@ident=$query]
    return
<MODULE_RESULT>
	<U>http://tei.oucs.ox.ac.uk/Query/tag.xq?name={data($i/@ident)}</U>
	<Title>
	  {name($i)}: {data($i/@ident)}
	</Title>
</MODULE_RESULT>}
</OneBoxResults>
return $R