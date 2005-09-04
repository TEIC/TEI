<elementSpec xmlns="http://www.tei-c.org/ns/1.0" module="corpus"
xml:id="PERSON" usage="opt" ident="person">
<equiv/>
<gloss/>
<desc>beskriver en deltagare i språklig interaktion. </desc>


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
    <desc>anger deltagarens roll i en grupp.</desc>


      <datatype><rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0"
name="datatype.Code"/></datatype>
      <valDesc>en uppsättning nyckelord som skall definieras</valDesc>

    </attDef>
    <attDef ident="sex" usage="opt">
    <equiv/>
    <desc>anger deltagarens kön.</desc>


      <datatype><rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0"
name="datatype.Sex"/></datatype>
      <valList type="closed">
        <valItem ident="m">

          <equiv/>
          <gloss>man</gloss>
        </valItem>
        <valItem ident="f">

          <equiv/>
          <gloss>kvinna</gloss>
        </valItem>
        <valItem ident="u">

          <equiv/>
          <gloss>okänd eller ej tillämpbar</gloss>
        </valItem>
      </valList>

    </attDef>
    <attDef ident="age" usage="opt">
    <equiv/>
    <desc>anger den åldersgrupp som deltagaren tillhör.</desc>
      <datatype><rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0"
name="datatype.Code"/></datatype>
      <valDesc>Föreslagna värden bör anges</valDesc>  
    </attDef>
  </attList>
    <exemplum>
    <egXML xmlns="http://www.tei-c.org/ns/Examples">
      <person sex="f" age="42">
        <p>Kvinnlig informat, välutbildad, född i Blida, Sverige den 12 januari 1950. Yrke okänt. 
          Talar flytande franska. Socioekonomisk grupp B2.</p>
      </person>
    </egXML>
  </exemplum>
  <remarks>
    <p rend="dataDesc">Kan innehålla en prosaliknande beskrivning indelad i stycken, eller valfria sekvenser av demografiska element i valfri kombination. </p>
  </remarks>
  <listRef>
    <ptr target="#CCAHPA"/>
  </listRef>
</elementSpec>