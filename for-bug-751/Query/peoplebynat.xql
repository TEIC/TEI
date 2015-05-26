for $t in  collection(""/db/protcem")//TEI.2[teiHeader//country[. &='England'] and text/body/div[@lang &= 'l_LA']]
order by $t//person[1]//surname
return
<stone>: {$t/@id}: 
{$t/teiHeader/fileDesc/sourceDesc/stoneDescription//zone/@target}, 
  {$t/teiHeader/fileDesc/sourceDesc/stoneDescription//form/@target}
 {
 for $person in $t//person return   
  <person>{$person/persName/surname/text()},
  {$person/persName/foreName/text()}</person>
 }
 {
 for  $ins in  $t//div
  return 
  <ins> {
  for $ab in $ins/ab/text()
   return
   <l>{$ab} </l> 
     } 
  </ins>
 }
</stone>
