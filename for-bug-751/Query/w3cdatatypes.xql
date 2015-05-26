declare namespace xs="http://www.w3.org/2001/XMLSchema";
<w3cdataList>
{
for $t in collection("/db/TEI")//xs:schema//xs:simpleType[@name]
order by $t/@name
return
<w3cdataType>{data($t/@name)}</w3cdataType>
}
</w3cdataList>
