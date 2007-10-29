declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace rng="http://relaxng.org/ns/structure/1.0";
declare namespace request="http://exist-db.org/xquery/request";
let $lang := request:get-parameter("lang", "")

<TEI xmlns="http://www.tei-c.org/ns/1.0" 
     xmlns:xi="http://www.w3.org/2001/XInclude"> 
<text>
<body>
<list type="ordered">
{
let $class := request:get-parameter("class", "")
for $c in collection("/db/TEI")//tei:classSpec[@ident=$class]
let $what:=$c/@ident
return
<item><hi>{data($what)}</hi>:
<list>
	{
for $e in
collection("/db/TEI")//tei:elementSpec[tei:classes/tei:memberOf[@key=$what]]
	return
	<item>{data($e/@ident)}: {    
    if ($t/tei:desc[@xml:lang=$lang]) then
        data($t/tei:desc[@xml:lang=$lang])
    else
        data($t/tei:desc[not(@xml:lang)])
}</item>
	}
</list>
</item>
}
</list>
</body>
</text>
</TEI> 