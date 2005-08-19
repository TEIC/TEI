<elementSpec xmlns="http://www.tei-c.org/ns/1.0" module="corpus"
xml:id="PERSON" usage="opt" ident="person">
<equiv/>
<gloss/>
<desc>opisuje pojedynczą osobę uczestniczącą w interakcji językowej</desc>
 
 
  <content>
    <rng:choice xmlns:rng="http://relaxng.org/ns/structure/1.0">
      <rng:oneOrMore>
        <rng:ref name="p"/>
      </rng:oneOrMore>
      <rng:zeroOrMore>
        <rng:ref name="tei.demographic"/>
      </rng:zeroOrMore>
    </rng:choice>
  </content>
  <attList>
    <attDef ident="role" usage="opt">
    <equiv/>
    <desc>określa rolę tego uczestnika w grupie</desc>
     
     
      <datatype><rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0"
name="datatype.Code"/></datatype>
      <valDesc>zestaw słów kluczowych do zdefiniowania</valDesc>
     
    </attDef>
    <attDef ident="sex" usage="opt">
    <equiv/>
    <desc>określa płeć uczestnika</desc>
     
     
      <datatype><rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0"
name="datatype.Sex"/></datatype>
      <valList type="closed">
        <valItem ident="m">
         
          <equiv/>
          <gloss>male</gloss>
        </valItem>
        <valItem ident="f">
         
          <equiv/>
          <gloss>female</gloss>
        </valItem>
        <valItem ident="u">
         
          <equiv/>
          <gloss>unknown or inapplicable</gloss>
        </valItem>
      </valList>
     
    </attDef>
    <attDef ident="age" usage="opt">
    <equiv/>
    <desc>określa grupę wiekową, do której należy uczestnik</desc>
      <datatype><rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0"
name="datatype.Code"/></datatype>
      <valDesc>sugerowane wartości zostaną podane</valDesc>  
    </attDef>
  </attList>
    <exemplum>
    <egXML xmlns="http://www.tei-c.org/ns/Examples">
      <person sex="f" age="42">
        <p>Female informant, well-educated, born in Shropshire
UK, 12 Jan 1950, of unknown occupation.
Speaks French fluently. Socio-Economic status B2.</p>
      </person>
    </egXML>
  </exemplum>
  <remarks>
    <p rend="dataDesc">Może zawierać opis zorganizowany w akapity lub
    dowolną sekwencję elementów nalężących do klasy demograficznej,
    zdefiniowanych w wytycznych TEI.</p>
  </remarks>
  <listRef>
    <ptr target="#CCAHPA"/>
  </listRef>
</elementSpec>
