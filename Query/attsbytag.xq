declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace rng="http://relaxng.org/ns/structure/1.0";
declare namespace request="http://exist-db.org/xquery/request";
let $e := request:request-parameter("name", "")
for $c in collection("/db/TEI")//(tei:elementSpec|tei:classSpec)[@ident=$e]
return
<Tag>
{
for $a in $c//tei:attDef
return 
 <att>
    <name>
    {$a/@usage}
    {data($a/@ident)}</name>
    <default>{data($a/tei:defaultVal)}</default>
    <datatype>{$a/tei:datatype/*}</datatype>
    <desc>{data($a/tei:desc)}</desc>
</att>
}
</Tag>