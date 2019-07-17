<?xml version="1.0" encoding="UTF-8"?>

<xsl:stylesheet version="2.0" xmlns:xd="http://www.pnp-software.com/XSLTdoc"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
  xmlns:tei="http://www.tei-c.org/ns/1.0" 
  xmlns:eg="http://www.tei-c.org/ns/Examples"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:i18n="i18n"
  exclude-result-prefixes="#all">
  
  <!-- named this variable $jtei.lang to avoid interference with $lang of the TEI framework -->
  <xsl:param name="jtei.lang" select="//tei:profileDesc/tei:langUsage/tei:language/@ident"/>    
  
  <!-- added a @pl attribute to include plural forms other than [stem + s] -->
  <xsl:variable name="i18n-lookup">
    <i18n>
      <entry xml:id="abstract-label">
        <text xml:lang="de">Zusammenfassung</text>
        <text xml:lang="en">Abstract</text>
        <text xml:lang="es">Resumen</text>
        <text xml:lang="fr">Résumé</text>
        <text xml:lang="it">Riassunto</text>        
        <text xml:lang="nl">Samenvatting</text>
        <text xml:lang="pt">Resumo</text>
      </entry>
      <entry xml:id="bibliography-label">
        <text xml:lang="de">Bibliografie</text>
        <text xml:lang="en">Bibliography</text>
        <text xml:lang="es">Bibliografía</text>
        <text xml:lang="fr">Bibliographie</text>
        <text xml:lang="it">Bibliografia</text>        
        <text xml:lang="nl">Bibliografie</text>
        <text xml:lang="pt">Bibliografia</text>
      </entry>
      <entry xml:id="author-label">
        <text xml:lang="de" pl="Autoren">Autor</text>
        <text xml:lang="en">Author</text>
        <text xml:lang="es" pl="Autores">Autor</text>
        <text xml:lang="fr">Auteur</text>
        <text xml:lang="it" pl="Autore">Autor</text>        
        <text xml:lang="nl">Auteur</text>
        <text xml:lang="pt" pl="Autore">Autor</text>
      </entry>
      <entry xml:id="keywords-label">
        <text xml:lang="de">Schlagwörter</text>
        <text xml:lang="en">Keywords</text>
        <text xml:lang="es">Palabras Clave</text>
        <text xml:lang="fr">Mots-clés</text>
        <text xml:lang="it">Parole Chiave</text>        
        <text xml:lang="nl">Sleutelbegrippen</text>
        <text xml:lang="pt">Palavras Chaves</text>
      </entry>
      <entry xml:id="appendixes-label">
        <text xml:lang="de">Anhänge</text>
        <text xml:lang="en">Appendixes</text>
        <text xml:lang="es">Apéndices</text>
        <text xml:lang="fr">Annexes</text>
        <text xml:lang="it">Appendices</text>        
        <text xml:lang="nl">Appendices</text>
        <text xml:lang="pt">Apêndices</text>
      </entry>
      <entry xml:id="appendix-label">
        <text xml:lang="de">Anhang</text>
        <text xml:lang="en">Appendix</text>
        <text xml:lang="es">Apéndice</text>
        <text xml:lang="fr">Annexe</text>
        <text xml:lang="it">Appendice</text>        
        <text xml:lang="nl">Appendix</text>
        <text xml:lang="pt">Apêndice</text>
      </entry>
      <entry xml:id="authorNotes-label">
        <text xml:lang="de">Anmerkungen des Autors</text>
        <text xml:lang="en">Author's Notes</text>
        <text xml:lang="es">Notas del Autor</text>
        <text xml:lang="fr">Note de l'Auteur</text>
        <text xml:lang="it">Note dell'Autore</text>        
        <text xml:lang="nl">Noot van de Auteur</text>        
        <text xml:lang="pt">Notas do Autor</text>
      </entry>
      <entry xml:id="editorNotes-label">
        <text xml:lang="de">Anmerkungen des Editors</text>
        <text xml:lang="en">Editor's Notes</text>
        <text xml:lang="es">Notas del Editor</text>
        <text xml:lang="fr">Note de l'Éditeur</text>
        <text xml:lang="it">Note dell'Editore</text>        
        <text xml:lang="nl">Noot van de Editeur</text>        
        <text xml:lang="pt">Notas do Editor</text>
      </entry>
      <entry xml:id="corrections-label">
        <text xml:lang="de">Errata</text>
        <text xml:lang="en">Errata</text>
        <text xml:lang="es">Errata</text>
        <text xml:lang="fr">Errata</text>
        <text xml:lang="it">Errata</text>        
        <text xml:lang="nl">Errata</text>        
        <text xml:lang="pt">Errata</text>
      </entry>
      <entry xml:id="dedication-label">
        <text xml:lang="de">Widmung</text>
        <text xml:lang="en">Dedication</text>
        <text xml:lang="es">Dedicación</text>
        <text xml:lang="fr">Dédicace</text>
        <text xml:lang="it">Dedizione</text>        
        <text xml:lang="nl">Opdracht</text>        
        <text xml:lang="pt">Dedicação</text>
      </entry>
      <entry xml:id="acknowledgements-label">
        <text xml:lang="de">Anerkennung</text>
        <text xml:lang="en">Acknowledgements</text>
        <text xml:lang="es">Reconocimiento</text>
        <text xml:lang="fr">Remerciements</text>
        <text xml:lang="it">Ringraziamenti</text>        
        <text xml:lang="nl">Dankwoord</text>        
        <text xml:lang="pt">Reconhecimento</text>
      </entry>
      <entry xml:id="notes-label">
        <text xml:lang="de">Anmerkungen</text>
        <text xml:lang="en">Notes</text>
        <text xml:lang="es">Notas</text>
        <text xml:lang="fr">Notes</text>
        <text xml:lang="it">Note</text>
        <text xml:lang="nl">Noten</text>
        <text xml:lang="pt">Notas</text>
      </entry>
      <entry xml:id="index-label">
        <text xml:lang="de">Index</text>
        <text xml:lang="en">Index</text>
        <text xml:lang="es">Índice</text>
        <text xml:lang="fr">Index</text>
        <text xml:lang="it">Indice</text>        
        <text xml:lang="nl">Index</text>
        <text xml:lang="pt">Índice</text>        
      </entry>
      <entry xml:id="and">
        <text xml:lang="de">und</text>
        <text xml:lang="en">and</text>
        <text xml:lang="es">y</text>
        <text xml:lang="fr">et</text>
        <text xml:lang="it">e</text>        
        <text xml:lang="nl">en</text>
        <text xml:lang="pt">e</text>        
      </entry>
      
      <entry xml:id="table-label">
        <text xml:lang="de" pl="Tabellen">Tabelle</text>
        <text xml:lang="en">table</text>
        <text xml:lang="es">tabla</text>
        <text xml:lang="fr" pl="tableaux">tableau</text>
        <text xml:lang="it" pl="tabelle">tabella</text>
        <text xml:lang="nl" pl="tabellen">tabel</text>
        <text xml:lang="pt">tabela</text>
      </entry>
      <entry xml:id="figure-label">
        <text xml:lang="de" pl="Abbildungen">Abbildung</text>
        <text xml:lang="en">figure</text>
        <text xml:lang="es">figura</text>
        <text xml:lang="fr">illustration</text>
        <text xml:lang="it" pl="figure">figura</text>
        <text xml:lang="nl" pl="afbeeldingen">afbeelding</text>
        <text xml:lang="pt">figura</text>
      </entry>
      <entry xml:id="section-label">
        <text xml:lang="de" pl="Abschnitte">Abschnitt</text>
        <text xml:lang="en">section</text>
        <text xml:lang="es" pl="secciónes">sección</text>
        <text xml:lang="fr">section</text>
        <text xml:lang="it" pl="sezioni">sezione</text>
        <text xml:lang="nl">sectie</text>
        <text xml:lang="pt" pl="seções">seção</text>
      </entry>
      <entry xml:id="example-label">
        <text xml:lang="de" pl="Beispiele">Beispiel</text>
        <text xml:lang="en">example</text>
        <text xml:lang="es">ejemplo</text>
        <text xml:lang="fr">exemple</text>
        <text xml:lang="it" pl="esempi">esempio</text>
        <text xml:lang="nl" pl="voorbeelden">voorbeeld</text>
        <text xml:lang="pt">exemplo</text>
      </entry>
      <entry xml:id="note-label">
        <text xml:lang="de" pl="Anmerkungen">Anmerkung</text>
        <text xml:lang="en">note</text>
        <text xml:lang="es">nota</text>
        <text xml:lang="fr">note</text>
        <text xml:lang="it" pl="note">nota</text>
        <text xml:lang="nl" pl="noten">note</text>
        <text xml:lang="pt">notas</text>
      </entry>
    </i18n>
  </xsl:variable>

  <xsl:key name="i18n" match="entry" use="@xml:id"/>
  <xsl:key name="plural-lookup" match="text" use="lower-case(.)"/>

  <xsl:function name="i18n:key">
    <xsl:param name="string"/>
    <xsl:copy-of select="i18n:key($string, $jtei.lang)"/>
  </xsl:function>

  <xsl:function name="i18n:key">
    <xsl:param name="string"/>
    <xsl:param name="lang"/>
    <xsl:value-of select="(key('i18n', $string, $i18n-lookup)/text[@xml:lang = $lang]/text(),concat('*',$string,'*'))[1]"/>
  </xsl:function>
  
  <xsl:function name="i18n:plural">
    <xsl:param name="string"/>
    <xsl:copy-of select="key('plural-lookup', $string, $i18n-lookup)[@xml:lang=$jtei.lang]"/>
  </xsl:function>
  
</xsl:stylesheet>
