declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace rng="http://relaxng.org/ns/structure/1.0";
declare namespace request="http://exist-db.org/xquery/request";
let $e := request:get-parameter("name", "")
let $lang := request:get-parameter("lang", "")
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
    <defaultVal>{data($a/tei:defaultVal)}</defaultVal>
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
    <desc>{    if ($t/tei:desc[@xml:lang=$lang]) then
        data($t/tei:desc[@xml:lang=$lang])
    else
        data($t/tei:desc[not(@xml:lang)])
	}</desc>
</att>
}
</Tag>