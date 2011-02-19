declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace rng="http://relaxng.org/ns/structure/1.0";
<teiModulesList>
{
for $t in collection("/db/TEI")//tei:moduleSpec[not(@type='decls')]
let $lang := request:get-parameter("lang", "en")
let $what:=$t/@ident
let $Desc:=
    if ($t/tei:desc[@xml:lang=$lang]) then
        $t/tei:desc[@xml:lang=$lang]
    else
        $t/tei:desc[not(@xml:lang)][1]
let $chapter:=$t/ancestor::tei:div[last()]/@xml:id
order by $t/@ident
return
  <teiModule>
   <moduleName>{data($what)}</moduleName>
   <moduleDesc>{data($Desc)}</moduleDesc>
   <moduleChapter>{data($chapter)}</moduleChapter>
  </teiModule>
}
</teiModulesList>
