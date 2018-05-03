declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace rng="http://relaxng.org/ns/structure/1.0";
<dataList>
{
for $t in collection("/db/TEI")//tei:dataSpec
order by $t/@ident
return
 <dataType>
  <dataName>{data($t/@ident)}</dataName>
  <dataDesc>{$t/tei:desc}</dataDesc>
 </dataType>
}
</dataList>

