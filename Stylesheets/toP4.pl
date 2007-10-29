while (<>) {
     s/tei:teiCorpus/teiCorpus.2/;
     s/tei:TEI/TEI.2/;
     s/TEI.1_/TEI.2.1_/;
     s/tei://g;
     s/\@xml:id/\@id/g;
     s/teiP4Compat">false</teiP4Compat">true</;
     print;
}
