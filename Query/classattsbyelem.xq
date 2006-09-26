declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace rng="http://relaxng.org/ns/structure/1.0";
declare namespace request="http://exist-db.org/xquery/request";
let $lang := request:get-parameter("lang", "")
<Element>
{
let $n := request:get-parameter("name", "")
for $c in
collection("/db/TEI")//tei:elementSpec[@ident=$n]/tei:classes/tei:memberOf
return
<Class>
{
for $a in collection("/db/TEI")//tei:classSpec[@ident=$c/@key or @ident='att.global']//tei:attDef
return
 <att>
    <name>
    {$a/@usage}
    {data($a/@ident)}</name>
    <default>{data($a/tei:defaultVal)}</default>
    <datatype>{
      string($a/tei:datatype/*)
     }
     </datatype>
     <valList>
       {
	for $v in $a/tei:valList/tei:valItem return
	<valItem>
	   {$v/@ident}
	</valItem>
       }
     </valList>	
    <desc>{
        if ($t/tei:desc[@xml:lang=$lang]) then
	   data($t/tei:desc[@xml:lang=$lang])
	else
	   data($t/tei:desc[not(@xml:lang)])
	}</desc>
  </att>
}
</Class>
}
</Element>
