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
for $a in collection("/db/TEI")//tei:classSpec[@ident=$c/@key]//tei:attDef
return
 <att>
    <name>
    {$a/@usage}
    {data($a/@ident)}</name>
    <desc>{
        if ($t/tei:desc[@xml:lang=$lang]) then
	   data($t/tei:desc[@xml:lang=$lang])
	else
	   data($t/tei:desc[not(@xml:lang)])
	}</desc>
    <defaultVal>{data($a/tei:defaultVal)}</default>
     { for $d in  $a/tei:datatype return
	 <datatype>
	    { $d/@minOccurs }
	    { $d/@maxOccurs }
	    { $d/* }
         </datatype>
     }
     {  
      for $d in $a/tei:valList[@type='closed']  return
       <valList>
         {for $dv in $d/tei:valItem return
             <valItem>
	        { $dv/@ident }
             </valItem>
             }
       </valList>
      }
  </att>
}
</Class>
}
</Element>
