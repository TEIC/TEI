xquery version "3.0";
declare default element namespace "http://www.w3.org/1999/xhtml";
declare namespace output = "http://www.w3.org/2010/xslt-xquery-serialization";
declare namespace tei = "http://www.tei-c.org/ns/1.0";
declare option output:method "html";
declare option output:indent "yes";
declare option output:omit-xml-declaration "yes";
(: 2025-06-24: Made by ebb to assist with TEI Guidelines ticket #2652: https://github.com/TEIC/TEI/issues/2652 :)
<html>
    <head>
        <title>Task Completion Table for Specs Lacking listRef Elements</title>
    </head>
    <body>
        <h1>A Task Completion Table for Specs that lack listRef</h1>
        <table>
            <tr>
                <th>#</th>
                <th>spec type</th>
                <th>spec name</th>
                <th>references</th>
                <th>Added listRef to Spec?</th>
                <th>Councilor initials</th>
            </tr>
            {
                let $p5xml := doc('../p5.xml')
                (: let $specs := $p5xml//*[self::tei:specGrp or self::tei:div]/*[local-name() ! ends-with(., 'Spec')]:)
                let $specs := collection('../Source/Specs/?select=*.xml')/*
                let $listRefless :=
                for $s in $specs ! local-name() => distinct-values()
                return
                    $specs[name() = $s][not(descendant::tei:listRef)]
                let $sortedlistRefless :=
                for $l in $listRefless
                    order by $l/name() || $l/@ident
                return
                    $l
                for $s at $pos in $sortedlistRefless
                return
                    <tr>
                        <td>{$pos}</td>
                        <td>{$s ! local-name()}</td>
                        <td>{$s/@ident ! normalize-space()}</td>
                        <td>
                            <ol>
                                {
                                    
                                    let $mentions := ($p5xml//*[. ! normalize-space() = $s/@ident] | $p5xml//*[@key = $s/@ident])
                                    let $usefulMentions := $mentions[not(ancestor::*[local-name() ! ends-with(., 'Spec')])]
                                    let $containers := $usefulMentions/ancestor::tei:div[@xml:id][1]
                                    for $c in $containers
                                    return
                                        <li>{'- [ ] ' || $c/@xml:id ! normalize-space() || ': ' || $c/tei:head ! normalize-space()}</li>
                                }
                            </ol>
                        </td>
                        <td><ul><li>{'- [ ] '}</li></ul></td>
                        <td>____</td>
                    </tr>
            }
        </table>
    </body>
</html>