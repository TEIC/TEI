<elementSpec xmlns="http://www.tei-c.org/ns/1.0" module="corpus"
 xml:id="PERSON" usage="opt" ident="person">
  <equiv/>
  <gloss/>
  <desc>describe un único participante en una interacción
lingüística.</desc>

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
    <desc>especifica el papel de este participante dentro del grupo.</desc>

    <datatype><rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0"
name="datatype.Code"/>
    </datatype>
    <valDesc>un conjunto de palabras clave a definir.</valDesc>

   </attDef>
   <attDef ident="sex" usage="opt">
     <equiv/>
     <desc>especifica el sexo del participante.</desc>
    <datatype><rng:ref xmlns:rng="http://relaxng.org/ns/structure/1.0"
 name="datatype.Sex"/></datatype>
    <valList type="closed">
      <valItem ident="m">
        <equiv/>
        <gloss>masculino</gloss>
      </valItem>
      <valItem ident="f">
        <equiv/>
        <gloss>femenino</gloss>
      </valItem>
      <valItem ident="u">
        <equiv/>
        <gloss>desconocido o inaplicable.</gloss>
      </valItem>
    </valList>
   </attDef>
 <attDef ident="age" usage="opt">
   <equiv/>
    <desc>especifica la edad del grupo al que pertenece el
participante.</desc>
    <datatype>
      <rng:ref  xmlns:rng="http://relaxng.org/ns/structure/1.0"
name="datatype.Code"/>
    </datatype>
    <valDesc>se deben añadir valores sugeridos.</valDesc>
   </attDef>
  </attList>
  <exemplum>
 <egXML xmlns="http://www.tei-c.org/ns/Examples">
    <person sex="f" age="42">
 <p>Informante femenina, bien educada, nacida en Shropshire, Reino Unido, el
12 de enero de 1950, cuya ocupación se desconoce. Habla francés de forma
fluida. Estatus socioeconómico B2.</p>
 </person>
 </egXML>
  </exemplum>
  <remarks>
 <p rend="dataDesc">Puede contener una descripción organizada en párrafos o
una serie de elementos demográficos combinados de cualquier modo.</p>
  </remarks>
  <listRef>
    <ptr target="#CCAHPA"/>
  </listRef>
</elementSpec>



