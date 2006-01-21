declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace rng="http://relaxng.org/ns/structure/1.0";

<Element>
{
let $n := request:request-parameter("name", "")
for $c in
collection("/db/TEI")//tei:elementSpec[@ident=$n]/tei:classes/tei:memberOf
return
<Class>
{
for $a in collection("/db/TEI")//tei:classSpec[@ident=$c/@key or @ident='tei.global']//tei:attDef
return
 <att>
    <name>
    {$a/@usage}
    {data($a/@ident)}</name>
    <default>{data($a/tei:defaultVal)}</default>
    <datatype>{
    if (tei:valList[@type='closed'])
    then
      'Closed list'
    else
      string($a/tei:datatype/*)
     }
     </datatype>
     <valList>
       {$a/tei:valList/*}
     </valList>	
    <desc>{data($a/tei:desc)}</desc>
  </att>
}
</Class>
}
</Element>