<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.w3.org/1999/xhtml" xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0" xmlns:html="http://www.w3.org/1999/xhtml" xmlns:rng="http://relaxng.org/ns/structure/1.0" xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:teix="http://www.tei-c.org/ns/Examples" xmlns:xhtml="http://www.w3.org/1999/xhtml" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" exclude-result-prefixes="xlink rng tei teix xhtml a html xs xsl" version="2.0">
  <xsl:param name="outputDir"><xsl:value-of select="$directory"/>/OPS</xsl:param>
  <xsl:key name="EXAMPLES" match="teix:*[ancestor::teix:egXML]" use="concat(ancestor::tei:div[last()]/@xml:id,local-name())"/>
  <xsl:key name="HEADS" match="tei:head" use="concat(@xml:lang,@corresp)"/>
  <xsl:key name="BACKLINKS" match="teix:egXML[@corresp]" use="substring(@corresp,2)"/>
  <xsl:key name="BACKLINKS" match="tei:ptr[@type='cit']" use="substring(@target,2)"/>
  <heads xmlns="http://www.tei-c.org/ns/1.0">
    <head xml:lang="fr" corresp="AB">À propos des Principes directeurs</head>
    <head xml:lang="fr" corresp="AI">Mécanismes analytiques simples</head>
    <head xml:lang="fr" corresp="BIB">Bibliographie</head>
    <head xml:lang="fr" corresp="CC">Corpus linguistiques</head>
    <head xml:lang="fr" corresp="CE">Degré de certitude et responsabilité</head>
    <head xml:lang="fr" corresp="CH">Langues et jeux de caractères </head>
    <head xml:lang="fr" corresp="CO">Éléments disponibles pour tous les documents TEI</head>
    <head xml:lang="fr" corresp="COL">Colophon</head>
    <head xml:lang="fr" corresp="dedication">Dédicace</head>
    <head xml:lang="fr" corresp="DI">Dictionnaires</head>
    <head xml:lang="fr" corresp="DR">Théâtre</head>
    <head xml:lang="fr" corresp="DS">Structure textuelle par défaut</head>
    <head xml:lang="fr" corresp="FS">Structures de traits</head>
    <head xml:lang="fr" corresp="FT">Tableaux, formules et graphiques</head>
    <head xml:lang="fr" corresp="GD">Graphes, réseaux et arbres</head>
    <head xml:lang="fr" corresp="HD">En-tête TEI</head>
    <head xml:lang="fr" corresp="MS">Description de manuscrits</head>
    <head xml:lang="fr" corresp="ND">Noms, dates, personnes et lieux</head>
    <head xml:lang="fr" corresp="NH">Structures non hiérarchiques</head>
    <head xml:lang="fr" corresp="PH">Représentation de sources primaires</head>
    <head xml:lang="fr" corresp="PREF">Préface et remerciements</head>
    <head xml:lang="fr" corresp="PREFS">Notes préliminaires </head>
    <head xml:lang="fr" corresp="REF-CLASSES-ATTS">Classes attributives</head>
    <head xml:lang="fr" corresp="REF-CLASSES-MODEL">Classes structurales</head>
    <head xml:lang="fr" corresp="REF-ELEMENTS">Éléments</head>
    <head xml:lang="fr" corresp="REF-MACROS">Types de données et autres macros </head>
    <head xml:lang="fr" corresp="SA">Liens, segmentation et alignement</head>
    <head xml:lang="fr" corresp="SG">Petite introduction à XML</head>
    <head xml:lang="fr" corresp="ST">Infrastructure de la TEI</head>
    <head xml:lang="fr" corresp="TC">Apparat critique</head>
    <head xml:lang="fr" corresp="TD">Éléments de déclaration d’un modèle</head>
    <head xml:lang="fr" corresp="TitlePageVerso">Versions des Principes directeurs</head>
    <head xml:lang="fr" corresp="TS">Transcriptions de la parole</head>
    <head xml:lang="fr" corresp="USE">Utiliser la TEI</head>
    <head xml:lang="fr" corresp="VE">Poésie</head>
    <head xml:lang="fr" corresp="WD">Représentation des caractères et
    des glyphes non standard</head>
    <head xml:lang="zh-TW" corresp="AI">簡易分析機制</head>
    <head xml:lang="zh-TW" corresp="CE">確定程度與不確定程度</head>
    <head xml:lang="zh-TW" corresp="CO">所有TEI文件所通用的元素</head>
    <head xml:lang="zh-TW" corresp="CC">文集文本</head>
    <head xml:lang="zh-TW" corresp="DI">紙本字典</head>
    <head xml:lang="zh-TW" corresp="DR">劇本</head>
    <head xml:lang="zh-TW" corresp="FT">表格、方程式與圖表</head>
    <head xml:lang="zh-TW" corresp="WD">文字與字體說明</head>
    <head xml:lang="zh-TW" corresp="HD">TEI標頭</head>
    <head xml:lang="zh-TW" corresp="FS">功能結構 (Feature Structures)</head>
    <head xml:lang="zh-TW" corresp="SA">Linking, segmentation and alignment連結、分割與隊列</head>
    <head xml:lang="zh-TW" corresp="MS">寫本描述</head>
    <head xml:lang="zh-TW" corresp="ND">名稱與日期</head>
    <head xml:lang="zh-TW" corresp="GD">圖形、網絡與樹狀結構</head>
    <head xml:lang="zh-TW" corresp="TS">轉錄的言詞</head>
    <head xml:lang="zh-TW" corresp="TD">TEI模組說明</head>
    <head xml:lang="zh-TW" corresp="ST">所有TEI模組可用的元素集、資料類型、巨集指令之宣告</head>
    <head xml:lang="zh-TW" corresp="TC">學術編輯註解</head>
    <head xml:lang="zh-TW" corresp="DS">預設文件結構</head>
    <head xml:lang="zh-TW" corresp="PH">原文轉錄</head>
    <head xml:lang="zh-TW" corresp="VE">韻文結構</head>
    <head xml:lang="zh-TW" corresp="dedication">獻辭</head>
    <head xml:lang="zh-TW" corresp="PREF">序言與致謝</head>
    <head xml:lang="zh-TW" corresp="PREFS">序文的言辭</head>
    <head xml:lang="zh-TW" corresp="REF-CLASSES-ATTS">屬性集</head>
    <head xml:lang="zh-TW" corresp="REF-CLASSES-MODEL">元素集</head>
    <head xml:lang="zh-TW" corresp="REF-ELEMENTS">元素</head>
    <head xml:lang="zh-TW" corresp="REF-MACROS">資料類型與其他巨集指令</head>
    <head xml:lang="zh-TW" corresp="USE">使用TEI</head>
    <head xml:lang="zh-TW" corresp="SG">XML入門</head>
    <head xml:lang="zh-TW" corresp="AB">關於</head>
    <head xml:lang="it" corresp="USE">Come usare TEI</head>
    <head xml:lang="it" corresp="SG">Una graduale introduzione a XML</head>
    <head xml:lang="it" corresp="AB">Le presenti Linee Guida</head>
    <head xml:lang="it" corresp="AI">Semplici meccanismi di analisi</head>
    <head xml:lang="it" corresp="CE">Certezza e incertezza</head>
    <head xml:lang="it" corresp="CO">Elementi comuni a tutti i documenti TEI</head>
    <head xml:lang="it" corresp="CC">Corpus di testi</head>
    <head xml:lang="it" corresp="DI">Dizionari a stampa</head>
    <head xml:lang="it" corresp="DR">Testi per performance</head>
    <head xml:lang="it" corresp="FT">Tabelle, formule e figure</head>
    <head xml:lang="it" corresp="WD">Documentazione di caratteri non standard e glifi</head>
    <head xml:lang="it" corresp="HD">L'intestazione TEI (TEI Header)</head>
    <head xml:lang="it" corresp="FS">Strutture di configurazione (feature structures)</head>
    <head xml:lang="it" corresp="SA">Collegamento, segmentazione e allineamento</head>
    <head xml:lang="it" corresp="MS">Descrizione di manoscritti</head>
    <head xml:lang="it" corresp="ND">Nomi e date</head>
    <head xml:lang="it" corresp="GD">Grafici, reti e alberi</head>
    <head xml:lang="it" corresp="TS">Trascrizione del parlato</head>
    <head xml:lang="it" corresp="TD">Documentazione dei moduli TEI</head>
    <head xml:lang="it" corresp="ST">Dichiarazione di classi, tipi di dati e macro
</head>
    <head xml:lang="it" corresp="TC">Apparato critico</head>
    <head xml:lang="it" corresp="DS">Struttura standard del testo</head>
    <head xml:lang="it" corresp="PH">Trascrizione di fonti primarie</head>
    <head xml:lang="it" corresp="VE">Strutture poetiche</head>
    <head xml:lang="it" corresp="COL">Colofone</head>
    <head xml:lang="it" corresp="dedication">Dedica</head>
    <head xml:lang="it" corresp="PREF">Prefazione e ringraziamenti</head>
    <head xml:lang="it" corresp="PREFS">Note introduttive</head>
    <head xml:lang="it" corresp="REF-CLASSES-ATTS">Classi di attributi</head>
    <head xml:lang="it" corresp="REF-CLASSES-MODEL">Classi di modelli</head>
    <head xml:lang="it" corresp="REF-ELEMENTS">Elementi</head>
    <head xml:lang="it" corresp="REF-MACROS">Datatype e altre macro</head>
    <head xml:lang="pt" corresp="AI">Mecanismos simples de análise</head>
    <head xml:lang="pt" corresp="CE">Certeza e incerteza</head>
    <head xml:lang="pt" corresp="CO">Elementos comuns a todos os documentos TEI</head>
    <head xml:lang="pt" corresp="CC">Textos do corpora</head>
    <head xml:lang="pt" corresp="DI">Dicionários impressos</head>
    <head xml:lang="pt" corresp="DR">Textos de actuação</head>
    <head xml:lang="pt" corresp="FT">Tabelas, fórmulas, e figuras</head>
    <head xml:lang="pt" corresp="WD">Documentação dos carateres</head>
    <head xml:lang="pt" corresp="HD">O cabeçalho TEI</head>
    <head xml:lang="pt" corresp="FS">Estrutura das características</head>
    <head xml:lang="pt" corresp="SA">Ligação, segmentação e alinhamento</head>
    <head xml:lang="pt" corresp="MS">Descrição do manuscrito</head>
    <head xml:lang="pt" corresp="ND">Nomes e datas</head>
    <head xml:lang="pt" corresp="GD">Grafos, redes, e árvores</head>
    <head xml:lang="pt" corresp="TS">Transcrição do discurso</head>
    <head xml:lang="pt" corresp="TD">Documentação dos módulos TEI</head>
    <head xml:lang="pt" corresp="ST">Declaraçoes de classes, tipos de dados, e macros disponíveis em todos os módulos  TEI </head>
    <head xml:lang="pt" corresp="TC">Critical Apparatus</head>
    <head xml:lang="pt" corresp="DS">Estrutura do texto por defeito</head>
    <head xml:lang="pt" corresp="PH">Transcrição de fontes primárias</head>
    <head xml:lang="pt" corresp="VE">Estrutura dos versos</head>
    <head xml:lang="pt" corresp="COL">Colophon</head>
    <head xml:lang="pt" corresp="dedication">Dedicação</head>
    <head xml:lang="pt" corresp="PREF">Prefácio e agradecimentos</head>
    <head xml:lang="pt" corresp="PREFS">Notas de prefácio</head>
    <head xml:lang="pt" corresp="REF-CLASSES-ATTS">Classes dos atributos</head>
    <head xml:lang="pt" corresp="REF-CLASSES-MODEL">Classes do modelo</head>
    <head xml:lang="pt" corresp="REF-ELEMENTS">Elementos</head>
    <head xml:lang="pt" corresp="REF-MACROS">Tipos de dados e outras macros</head>
    <!-- keywords on the interface for Module -->
    <head xml:lang="ja" corresp="AI">分析モジュール</head>
    <head xml:lang="ja" corresp="CE">確信度モジュール</head>
    <head xml:lang="ja" corresp="CO">コアモジュール</head>
    <head xml:lang="ja" corresp="CC">コーパスモジュール</head>
    <head xml:lang="ja" corresp="DI">辞書モジュール</head>
    <head xml:lang="ja" corresp="DR">舞台芸術モジュール</head>
    <head xml:lang="ja" corresp="FT">図表式モジュール</head>
    <head xml:lang="ja" corresp="WD">外字モジュール</head>
    <head xml:lang="ja" corresp="HD">ヘダーモジュール</head>
    <head xml:lang="ja" corresp="FS">素性構造モジュール</head>
    <head xml:lang="ja" corresp="SA">リンクモジュール</head>
    <head xml:lang="ja" corresp="MS">手書きモジュール</head>
    <head xml:lang="ja" corresp="ND">名前モジュール</head>
    <head xml:lang="ja" corresp="GD">グラフモジュール</head>
    <head xml:lang="ja" corresp="TS">発話モジュール</head>
    <head xml:lang="ja" corresp="TD">タグ定義モジュール</head>
    <head xml:lang="ja" corresp="TC">校勘モジュール</head>
    <head xml:lang="ja" corresp="DS">テキスト構造モジュール</head>
    <head xml:lang="ja" corresp="PH">転記モジュール</head>
    <head xml:lang="ja" corresp="VE">韻文モジュール</head>
    <!-- keywords on the interface for toc -->
    <head xml:lang="ja" corresp="COL">コロフォン</head>
    <head xml:lang="ja" corresp="dedication">献辞</head>
    <head xml:lang="ja" corresp="PREF">序文と謝辞</head>
    <head xml:lang="ja" corresp="PREFS">序文集</head>
    <head xml:lang="ja" corresp="REF-CLASSES-ATTS">属性クラス</head>
    <head xml:lang="ja" corresp="REF-CLASSES-MODEL">モデルクラス</head>
    <head xml:lang="ja" corresp="REF-ELEMENTS">要素</head>
    <!-- keywords on the interface for app.D -->
    <head xml:lang="ja" corresp="REF-MACROS">データ型とマクロ</head>
    <head xml:lang="ja" corresp="ST">TEIの基礎構造</head>
    <head xml:lang="ja" corresp="AB">本ガイドラインについて</head>
    <head xml:lang="ja" corresp="SG">XML入門</head>
    <head xml:lang="ja" corresp="USE">TEIの使い方</head>
    <head xml:lang="es" corresp="AI">Mecanismos analíticos simples</head>
    <head xml:lang="es" corresp="CE">Certeza e incertidumbre</head>
    <head xml:lang="es" corresp="CO">Elementos comunes a todos los documentos TEI</head>
    <head xml:lang="es" corresp="CC">Textos de corpus</head>
    <head xml:lang="es" corresp="DI">Diccionarios impresos</head>
    <head xml:lang="es" corresp="DR">Textos teatrales</head>
    <head xml:lang="es" corresp="FT">Tablas, fórmulas y figuras</head>
    <head xml:lang="es" corresp="WD">Documentación de caracteres y glifos</head>
    <head xml:lang="es" corresp="HD">La cabecera TEI</head>
    <head xml:lang="es" corresp="FS">Estructuras de rasgos</head>
    <head xml:lang="es" corresp="SA">Enlace, segmentación y alineación</head>
    <head xml:lang="es" corresp="MS">Descripción de manuscritos</head>
    <head xml:lang="es" corresp="ND">Nombres y fechas</head>
    <head xml:lang="es" corresp="GD">Grafos, redes y árboles</head>
    <head xml:lang="es" corresp="TS">Habla transcrita</head>
    <head xml:lang="es" corresp="TD">Documentación de los módulos de TEI</head>
    <head xml:lang="es" corresp="ST">Declaraciones de clases, tipos de datos y macros disponibles para todos los módulos TEI</head>
    <head xml:lang="es" corresp="TC">Aparato crítico</head>
    <head xml:lang="es" corresp="DS">Estructura textual por defecto</head>
    <head xml:lang="es" corresp="PH">Transcripción de fuentes primarias</head>
    <head xml:lang="es" corresp="VE">Estructuras de versos</head>
    <head xml:lang="es" corresp="AB">Sobre estas directrices</head>
    <head xml:lang="es" corresp="REF-CLASSES-ATTS">Clases de atributos</head>
    <head xml:lang="es" corresp="REF-CLASSES-MODEL">Clases de modelos</head>
    <head xml:lang="es" corresp="REF-ELEMENTS">Elementos</head>
    <head xml:lang="es" corresp="SG">Una introducción suave a XML</head>
    <head xml:lang="es" corresp="USE">Cómo usar TEI</head>
    <!-- Catalan -->
    <head xml:lang="ca" corresp="AI">Mecanismes analítics simples</head>
    <head xml:lang="ca" corresp="CE">Certitud i incertitud</head>
    <head xml:lang="ca" corresp="CO">Elements comuns a tots els documents TEI</head>
    <head xml:lang="ca" corresp="CC">Textos de corpus</head>
    <head xml:lang="ca" corresp="DI">Diccionaris impresos</head>
    <head xml:lang="ca" corresp="DR">Textos teatrals</head>
    <head xml:lang="ca" corresp="FT">Taules, fórmules i figures</head>
    <head xml:lang="ca" corresp="WD">Documentació de caràcters i glifs</head>
    <head xml:lang="ca" corresp="HD">L'encapçalament TEI</head>
    <head xml:lang="ca" corresp="FS">Estructures de trets</head>
    <head xml:lang="ca" corresp="SA">Enllaç, segmentació i alineació</head>
    <head xml:lang="ca" corresp="MS">Descripció de manuscrits</head>
    <head xml:lang="ca" corresp="ND">Noms i dates</head>
    <head xml:lang="ca" corresp="GD">Grafs, xarxes i arbres</head>
    <head xml:lang="ca" corresp="TS">Parla transcrita</head>
    <head xml:lang="ca" corresp="TD">Documentació dels mòduls de TEI</head>
    <head xml:lang="ca" corresp="ST">Declaracions de classes, tipus de dades i macros disponibles per a tots els mòduls TEI</head>
    <head xml:lang="ca" corresp="TC">Aparat crític</head>
    <head xml:lang="ca" corresp="DS">Estructura textual per defecte</head>
    <head xml:lang="ca" corresp="PH">Transcripció de fonts primàries</head>
    <head xml:lang="ca" corresp="VE">Estructures de versos</head>
    <head xml:lang="ca" corresp="AB">Sobre aquestes directrius</head>
    <head xml:lang="ca" corresp="REF-CLASSES-ATTS">Classes d'atributs</head>
    <head xml:lang="ca" corresp="REF-CLASSES-MODEL">Classes de models</head>
    <head xml:lang="ca" corresp="REF-ELEMENTS">Elements</head>
    <head xml:lang="ca" corresp="SG">Una introducció suau a XML</head>
    <head xml:lang="ca" corresp="USE">Com usar TEI</head>
    <!-- German -->
    <head xml:lang="de" corresp="AI">Einfache Analyse-Mechanismen</head>
    <head xml:lang="de" corresp="CE">Zuverlässigkeit und Unsicherheit</head>
    <head xml:lang="de" corresp="CO">Elemente, verfügbar in allen TEI-Dokumenten</head>
    <head xml:lang="de" corresp="CC">Korpustexte</head>
    <head xml:lang="de" corresp="DI">Gedruckte Wörterbücher</head>
    <head xml:lang="de" corresp="DR">Zur Aufführung bestimmte Texte</head>
    <head xml:lang="de" corresp="FT">Tabellen, Formeln und Abbildungen</head>
    <head xml:lang="de" corresp="WD">Dokumentation von Zeichen und Glyphen</head>
    <head xml:lang="de" corresp="HD">Der TEI-Header</head>
    <head xml:lang="de" corresp="FS">Merkmalsstrukturen</head>
    <head xml:lang="de" corresp="SA">Verlinken, Segmentieren und Parallelisieren</head>
    <head xml:lang="de" corresp="MS">Handschriftenbeschreibung</head>
    <head xml:lang="de" corresp="ND">Namen und Datumsangaben</head>
    <head xml:lang="de" corresp="GD">Graphen, Netze und Bäume</head>
    <head xml:lang="de" corresp="TS">Transkription gesprochener Sprache</head>
    <head xml:lang="de" corresp="TD">Dokumentation zu den TEI-Modulen</head>
    <head xml:lang="de" corresp="ST">Deklaration zu Klassen, Datentypen und Makros, verfügbar in allen TEI-Modulen</head>
    <head xml:lang="de" corresp="TC">Kritische Apparate</head>
    <head xml:lang="de" corresp="DS">Standard-Textstruktur</head>
    <head xml:lang="de" corresp="PH">Transkription von Primärquellen</head>
    <head xml:lang="de" corresp="VE">Vers-Strukturen</head>
    <head xml:lang="kr" corresp="AI">단순 분석 기제</head>
    <head xml:lang="kr" corresp="CE">확실성과 불확실성</head>
    <head xml:lang="kr" corresp="CO">모든 TEI 문서에 공통된 요소들</head>
    <head xml:lang="kr" corresp="CC">코퍼스 텍스트</head>
    <head xml:lang="kr" corresp="DI">인쇄 사전</head>
    <head xml:lang="kr" corresp="DR">공연 텍스트</head>
    <head xml:lang="kr" corresp="FT">표, 식, 그림</head>
    <head xml:lang="kr" corresp="WD">문자와 그림문자 문서</head>
    <head xml:lang="kr" corresp="HD">TEI 헤더</head>
    <head xml:lang="kr" corresp="FS">자질 구조</head>
    <head xml:lang="kr" corresp="SA">연결, 분할, 정렬</head>
    <head xml:lang="kr" corresp="MS">필사본 기술</head>
    <head xml:lang="kr" corresp="ND">이름과 날짜</head>
    <head xml:lang="kr" corresp="GD">그래프, 망, 수형도</head>
    <head xml:lang="kr" corresp="TS">전사된 대화(말)</head>
    <head xml:lang="kr" corresp="TD">TEI 모듈 문서</head>
    <head xml:lang="kr" corresp="ST">모든 TEI 모듈에 사용되는 부류, 자료유형, 마크로를 위한 선언</head>
    <head xml:lang="kr" corresp="TC">비평적 주석</head>
    <head xml:lang="kr" corresp="DS">기본값 텍스트 구조</head>
    <head xml:lang="kr" corresp="PH">일차 원전 전사</head>
    <head xml:lang="kr" corresp="VE">운문 구조</head>
    <head xml:lang="kr" corresp="COL">판권 면</head>
    <head xml:lang="kr" corresp="dedication">헌정</head>
    <head xml:lang="kr" corresp="PREF">서문과 사사 </head>
    <head xml:lang="kr" corresp="PREFS">서문</head>
    <head xml:lang="kr" corresp="REF-CLASSES-ATTS">속성 부류</head>
    <head xml:lang="kr" corresp="REF-CLASSES-MODEL">모형 부류</head>
    <head xml:lang="kr" corresp="REF-ELEMENTS">요소</head>
    <head xml:lang="kr" corresp="REF-MACROS">자료유형과 다른 마크로</head>
  </heads>
  <xsl:template name="processTEIHook">
    <xsl:for-each select="key('ELEMENTDOCS',1)">
      <xsl:variable name="me" select="@ident"/>
      <xsl:variable name="documentationLanguage">
        <xsl:call-template name="generateDocumentationLang"/>
      </xsl:variable>
      <xsl:variable name="langs">
        <xsl:value-of select="concat(normalize-space($documentationLanguage),' ')"/>
      </xsl:variable>
      <xsl:result-document doctype-public="{$doctypePublic}" doctype-system="{$doctypeSystem}" encoding="{$outputEncoding}" href="{$outputDir}/examples-{$me}.html" method="{$outputMethod}">
        <html>
          <xsl:comment>THIS IS A GENERATED FILE. DO NOT EDIT (10) </xsl:comment>
          <head>
            <title>
              <xsl:call-template name="i18n">
                <xsl:with-param name="word">Example</xsl:with-param>
              </xsl:call-template>
              <xsl:text>: &lt;</xsl:text>
              <xsl:value-of select="$me"/>
              <xsl:text>&gt; </xsl:text>
              <xsl:call-template name="makeGloss">
                <xsl:with-param name="langs" select="$langs"/>
              </xsl:call-template>
            </title>
            <xsl:call-template name="includeCSS"/>
            <meta content="Text Encoding Initiative Consortium XSLT
			   stylesheets" name="generator"/>
	    <xsl:call-template name="metaHTML">
	      <xsl:with-param name="title">
                <xsl:call-template name="i18n">
                  <xsl:with-param name="word">Example</xsl:with-param>
                </xsl:call-template>
                <xsl:text>: </xsl:text>
                <xsl:value-of select="$me"/>
                <xsl:text> </xsl:text>
                <xsl:call-template name="makeGloss">
                  <xsl:with-param name="langs" select="$langs"/>
                </xsl:call-template>
	      </xsl:with-param>
	    </xsl:call-template>
            <xsl:call-template name="includeJavascript"/>
            <xsl:call-template name="javascriptHook"/>
          </head>
          <body id="TOP">
            <xsl:call-template name="guidelinesTop">
              <xsl:with-param name="name">
                <xsl:call-template name="i18n">
                  <xsl:with-param name="word">Example</xsl:with-param>
                </xsl:call-template>
                <xsl:text>: &lt;</xsl:text>
                <xsl:value-of select="$me"/>
                <xsl:text>&gt; </xsl:text>
                <xsl:call-template name="makeGloss">
                  <xsl:with-param name="langs" select="$langs"/>
                </xsl:call-template>
              </xsl:with-param>
            </xsl:call-template>
            <div class="main-content">
              <xsl:call-template name="startDivHook"/>
              <h3>
                <xsl:call-template name="i18n">
                  <xsl:with-param name="word">Example</xsl:with-param>
                </xsl:call-template>
                <xsl:text>: &lt;</xsl:text>
                <xsl:value-of select="$me"/>
                <xsl:text>&gt; </xsl:text>
                <xsl:call-template name="makeGloss">
                  <xsl:with-param name="langs" select="$langs"/>
                </xsl:call-template>
              </h3>
              <p>These search results reproduce every example of the
	      use of <xsl:text>&lt;</xsl:text>
	      <xsl:value-of select="$me"/>
	      <xsl:text>&gt;</xsl:text> in the Guidelines, including all localised
	      and translated versions. In some cases, the examples have been drawn
	      from discussion of other elements in the Guidelines and illustrating
	      the use of <xsl:text>&lt;</xsl:text>
	      <xsl:value-of select="$me"/>
	      <xsl:text>&gt;</xsl:text>
	      is not the main 
	      focus of the passage in question. In other cases, examples may be direct 
	      translations of each other, and hence identical from the perspective of 
	      their encoding.</p>
              <xsl:variable name="items">
                <xsl:for-each select="/tei:TEI/tei:text/tei:body/tei:div">
                  <xsl:if test="count(key('EXAMPLES',concat(@xml:id,$me)))&gt;0">
                    <li>
                      <a href="#{@xml:id}">
                        <xsl:call-template name="header"/>
                      </a>
                    </li>
                  </xsl:if>
                </xsl:for-each>
              </xsl:variable>
              <xsl:if test="count($items/html:li)&gt;0">
                <ul>
                  <xsl:copy-of select="$items/html:li"/>
                </ul>
              </xsl:if>
              <xsl:for-each select="/tei:TEI/tei:text/tei:body/tei:div">
                <xsl:if test="count(key('EXAMPLES',concat(@xml:id,$me)))&gt;0">
                  <h4 id="{@xml:id}">
                    <xsl:call-template name="header"/>
                  </h4>
                  <xsl:for-each select="key('EXAMPLES',concat(@xml:id,$me))">
                    <xsl:variable name="pos">
                      <xsl:number level="any" from="teix:egXML"/>
                    </xsl:variable>
                    <xsl:if test="number($pos)=1 or ($me='egXML' and         number($pos)=2)">
                      <hr/>
                      <p>
                        <xsl:choose>
                          <xsl:when test="ancestor::tei:elementSpec">
                            <a href="ref-{ancestor::tei:elementSpec/@ident}.html">
                              <xsl:text>&lt;</xsl:text>
                              <xsl:value-of select="ancestor::tei:elementSpec/@ident"/>
                              <xsl:text>&gt;</xsl:text>
                            </a>
                          </xsl:when>
                          <xsl:when test="ancestor::tei:classSpec">
                            <a href="ref-{ancestor::tei:classSpec/@ident}.html">
                              <xsl:value-of select="ancestor::tei:classSpec/@ident"/>
                            </a>
                          </xsl:when>
                          <xsl:otherwise>
                            <xsl:for-each select="ancestor::tei:div[@xml:id and tei:head][1]">
                              <a>
                                <xsl:attribute name="href">
                                  <xsl:apply-templates select="." mode="generateLink"/>
                                </xsl:attribute>
                                <xsl:call-template name="header"/>
                              </a>
                            </xsl:for-each>
                          </xsl:otherwise>
                        </xsl:choose>
                      </p>
                      <xsl:apply-templates select="ancestor::teix:egXML">
                        <xsl:with-param name="simple">true</xsl:with-param>
                        <xsl:with-param name="highlight">
                          <xsl:value-of select="$me"/>
                        </xsl:with-param>
                      </xsl:apply-templates>
                    </xsl:if>
                  </xsl:for-each>
                </xsl:if>
              </xsl:for-each>
            </div>
            <xsl:call-template name="stdfooter">
              <xsl:with-param name="file">
                <xsl:text>examples-</xsl:text>
                <xsl:value-of select="$me"/>
              </xsl:with-param>
            </xsl:call-template>
          </body>
        </html>
      </xsl:result-document>
    </xsl:for-each>
  </xsl:template>
  <xsl:template name="metaHTML">
    <xsl:param name="title"/>
    <meta>
      <xsl:attribute name="{if ($outputTarget='html5') then 'property' else 'name'}">Language</xsl:attribute>
      <xsl:attribute name="content" select="$documentationLanguage"/>
    </meta>
    <meta>
      <xsl:attribute name="{if ($outputTarget='html5') then 'property' else 'name'}">DC.Title</xsl:attribute>
      <xsl:attribute name="content" select="$title"/>
    </meta>
    <meta>
      <xsl:attribute name="{if ($outputTarget='html5') then 'property' else 'name'}">DC.Language</xsl:attribute>
      <xsl:attribute name="content">SCHEME=iso639 <xsl:value-of select="$documentationLanguage"/></xsl:attribute>
    </meta>
    <meta>
	<xsl:attribute name="{if ($outputTarget='html5') then 'property' else 'name'}">DC.Creator.Address</xsl:attribute>
	<xsl:attribute name="content">tei@oucs.ox.ac.uk</xsl:attribute>
    </meta>
    <xsl:choose>
      <xsl:when test="$outputTarget='html5' or $outputTarget='epub3'">
	<meta charset="{$outputEncoding}"/>
      </xsl:when>
      <xsl:otherwise>
	<meta http-equiv="Content-Type" content="text/html; charset={$outputEncoding}"/>
      </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <xsl:template name="startDivHook">
      <xsl:if test="($outputTarget='epub' or $outputTarget='epub3') and not(parent::tei:div)">
        <h2>
          <xsl:call-template name="header"/>
        </h2>
      </xsl:if>
    <xsl:if test="not(parent::tei:div) or not(local-name(preceding::*[1])='head')">
      <div>
        <xsl:if test="$outputTarget='epub' or $outputTarget='epub3'">
          <xsl:attribute name="style">
            <xsl:text>margin-top: 0em;</xsl:text>
          </xsl:attribute>
        </xsl:if>
        <xsl:choose>
          <xsl:when test="not(parent::tei:div) and child::tei:div">
            <xsl:attribute name="class">
              <xsl:text>miniTOC miniTOC_left</xsl:text>
            </xsl:attribute>
            <xsl:call-template name="subtoc"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:attribute name="class">
              <xsl:text>miniTOC miniTOC_right</xsl:text>
            </xsl:attribute>
          </xsl:otherwise>
        </xsl:choose>
        <ul class="subtoc">
          <li class="subtoc">
            <xsl:call-template name="previousLink"/>
          </li>
          <li class="subtoc">
            <xsl:call-template name="nextLink"/>
          </li>
          <li class="subtoc">
            <a class="navigation" href="index.html">
              <xsl:call-template name="i18n">
                <xsl:with-param name="word">homeWord</xsl:with-param>
              </xsl:call-template>
            </a>
            <xsl:text> | </xsl:text>
            <a class="navigation" href="index-toc.html">
              <xsl:call-template name="i18n">
                <xsl:with-param name="word">tocWords</xsl:with-param>
              </xsl:call-template>
            </a>
          </li>
          <li class="subtoc">
            <xsl:choose>
              <xsl:when test="self::tei:elementSpec">
                <a class="navigation" href="REF-ELEMENTS.html">
                  <xsl:call-template name="showHead">
                    <xsl:with-param name="ID">
                      <xsl:text>REF-ELEMENTS</xsl:text>
                    </xsl:with-param>
                  </xsl:call-template>
                </a>
              </xsl:when>
              <xsl:when test="self::tei:classSpec[@type='model']">
                <a class="navigation" href="REF-CLASSES-MODEL.html">
                  <xsl:call-template name="showHead">
                    <xsl:with-param name="ID">
                      <xsl:text>REF-CLASSES-MODEL</xsl:text>
                    </xsl:with-param>
                  </xsl:call-template>
                </a>
              </xsl:when>
              <xsl:when test="self::tei:classSpec[@type='atts']">
                <a class="navigation" href="REF-CLASSES-ATTS.html">
                  <xsl:call-template name="showHead">
                    <xsl:with-param name="ID">
                      <xsl:text>REF-CLASSES-ATTS</xsl:text>
                    </xsl:with-param>
                  </xsl:call-template>
                </a>
              </xsl:when>
              <xsl:when test="self::tei:macroSpec">
                <a class="navigation" href="REF-MACROS.html">
                  <xsl:call-template name="showHead">
                    <xsl:with-param name="ID">
                      <xsl:text>REF-MACROS</xsl:text>
                    </xsl:with-param>
                  </xsl:call-template>
                </a>
              </xsl:when>
            </xsl:choose>
          </li>
        </ul>
      </div>
    </xsl:if>
  </xsl:template>
  <xsl:template name="mainPage">
    <xsl:param name="currentID"/>
    <xsl:call-template name="guidelinesTop"/>
    <div id="onecol" class="main-content">
      <xsl:call-template name="mainFrame">
        <xsl:with-param name="currentID" select="$currentID"/>
        <xsl:with-param name="minimal">true</xsl:with-param>
      </xsl:call-template>
      <xsl:if test="$currentID=''">
        <div style="float:left; margin:4%;">
          <h3>
            <xsl:call-template name="i18n">
              <xsl:with-param name="word">Versions of the Guidelines</xsl:with-param>
            </xsl:call-template>
          </h3>
          <ul>
            <li>
              <a href="index-toc.html">
                <xsl:call-template name="i18n">
                  <xsl:with-param name="word">tocWords</xsl:with-param>
                </xsl:call-template>
              </a>
            </li>
            <xsl:if test="$outputTarget='html'">
              <li>
                <a href="../../en/Guidelines.pdf">PDF</a>
              </li>
            </xsl:if>
            <li>
              <a href="http://www.tei-c.org/Council/tcw06.xml">
                <xsl:call-template name="i18n">
                  <xsl:with-param name="word">Getting the most recent version</xsl:with-param>
                </xsl:call-template>
              </a>
            </li>
            <li>
              <a href="http://tei.svn.sourceforge.net/viewvc/tei/">
                <xsl:call-template name="i18n">
                  <xsl:with-param name="word">Sourceforge Subversion Repository</xsl:with-param>
                </xsl:call-template>
              </a>
            </li>
            <li>
              <a href="http://sourceforge.net/tracker/?group_id=106328&amp;func=browse">
                <xsl:call-template name="i18n">
                  <xsl:with-param name="word">BugsFeatures</xsl:with-param>
                </xsl:call-template>
              </a>
            </li>
          </ul>
        </div>
        <div style="float:left; margin:4%;">
          <h3>
            <xsl:call-template name="i18n">
              <xsl:with-param name="word">Some Popular Sections</xsl:with-param>
            </xsl:call-template>
          </h3>
          <ul>
            <li>
              <a href="AB.html">
                <xsl:call-template name="showHead">
                  <xsl:with-param name="ID">
                    <xsl:text>AB</xsl:text>
                  </xsl:with-param>
                </xsl:call-template>
              </a>
            </li>
            <li>
              <a href="SG.html">
                <xsl:call-template name="showHead">
                  <xsl:with-param name="ID">
                    <xsl:text>SG</xsl:text>
                  </xsl:with-param>
                </xsl:call-template>
              </a>
            </li>
            <li>
              <a href="ST.html">
                <xsl:call-template name="showHead">
                  <xsl:with-param name="ID">
                    <xsl:text>ST</xsl:text>
                  </xsl:with-param>
                </xsl:call-template>
              </a>
            </li>
            <li>
              <a href="HD.html">
                <xsl:call-template name="showHead">
                  <xsl:with-param name="ID">
                    <xsl:text>HD</xsl:text>
                  </xsl:with-param>
                </xsl:call-template>
              </a>
            </li>
            <li>
              <a href="CO.html">
                <xsl:call-template name="showHead">
                  <xsl:with-param name="ID">
                    <xsl:text>CO</xsl:text>
                  </xsl:with-param>
                </xsl:call-template>
              </a>
            </li>
            <li>
              <a href="USE.html">
                <xsl:call-template name="showHead">
                  <xsl:with-param name="ID">
                    <xsl:text>USE</xsl:text>
                  </xsl:with-param>
                </xsl:call-template>
              </a>
            </li>
            <li>
              <a href="REF-CLASSES-MODEL.html">
                <xsl:call-template name="showHead">
                  <xsl:with-param name="ID">
                    <xsl:text>REF-CLASSES-MODEL</xsl:text>
                  </xsl:with-param>
                </xsl:call-template>
              </a>
            </li>
            <li>
              <a href="REF-CLASSES-ATTS.html">
                <xsl:call-template name="showHead">
                  <xsl:with-param name="ID">
                    <xsl:text>REF-CLASSES-ATTS</xsl:text>
                  </xsl:with-param>
                </xsl:call-template>
              </a>
            </li>
            <li>
              <a href="REF-ELEMENTS.html">
                <xsl:call-template name="showHead">
                  <xsl:with-param name="ID">
                    <xsl:text>REF-ELEMENTS</xsl:text>
                  </xsl:with-param>
                </xsl:call-template>
              </a>
            </li>
          </ul>
        </div>
        <xsl:variable name="name">TEI Guidelines TOC </xsl:variable>
        <xsl:variable name="outName">
          <xsl:call-template name="outputChunkName">
            <xsl:with-param name="ident">
              <xsl:text>index-toc</xsl:text>
            </xsl:with-param>
          </xsl:call-template>
        </xsl:variable>
        <xsl:if test="$verbose='true'">
          <xsl:message>Opening file <xsl:value-of select="$outName"/></xsl:message>
        </xsl:if>
        <xsl:result-document 
	    doctype-public="{$doctypePublic}"
	    omit-xml-declaration="yes"
	    doctype-system="{$doctypeSystem}" 
	    encoding="{$outputEncoding}" 
	    href="{$outName}" 
	    method="{$outputMethod}">
          <html>
            <xsl:comment>THIS IS A GENERATED FILE. DO NOT EDIT (8) </xsl:comment>
            <head>
              <title>
                <xsl:value-of select="$name"/>
              </title>
              <xsl:call-template name="includeCSS"/>
              <meta content="Text Encoding Initiative Consortium XSLT
			     stylesheets" name="generator"/>
		<xsl:call-template name="metaHTML">
		  <xsl:with-param name="title" select="$name"/>
		</xsl:call-template>
              <xsl:call-template name="includeJavascript"/>
              <xsl:call-template name="javascriptHook"/>
            </head>
            <body id="TOP">
              <xsl:call-template name="bodyHook"/>
              <xsl:call-template name="guidelinesTop"/>
              <div id="onecol" class="main-content">
                <xsl:call-template name="mainTOC"/>
              </div>
              <xsl:call-template name="stdfooter"/>
            </body>
          </html>
        </xsl:result-document>
        <xsl:if test="$verbose='true'">
          <xsl:message>Closing file <xsl:value-of select="$outName"/></xsl:message>
        </xsl:if>
      </xsl:if>
    </div>
  </xsl:template>
  <xsl:template name="showHead">
    <xsl:param name="ID"/>
    <xsl:variable name="Here" select="/"/>
    <xsl:for-each select="id($ID)">
      <xsl:choose>
        <xsl:when test="ancestor::tei:front">
          <xsl:number format="i"/>
        </xsl:when>
        <xsl:when test="ancestor::tei:back">
          <xsl:number format="A"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:number/>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:text> </xsl:text>
    </xsl:for-each>
    <xsl:for-each select="document('')">
      <xsl:choose>
        <xsl:when test="key('HEADS',concat($documentationLanguage,$ID))">
          <xsl:for-each select="key('HEADS',concat($documentationLanguage,$ID))">
            <xsl:value-of select="."/>
          </xsl:for-each>
        </xsl:when>
        <xsl:otherwise>
          <xsl:for-each select="$Here">
            <xsl:for-each select="id($ID)">
              <xsl:value-of select="tei:head"/>
            </xsl:for-each>
          </xsl:for-each>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>
  <xsl:template name="numberFrontDiv">
    <xsl:param name="minimal"/>
    <xsl:if test="count(ancestor::tei:div)&lt;2">
      <xsl:number count="tei:div" format="i.1.1." level="multiple"/>
      <xsl:if test="$minimal='false'">
        <xsl:value-of select="$numberSpacer"/>
      </xsl:if>
    </xsl:if>
  </xsl:template>
  <xsl:template name="pageHeader">
    <xsl:param name="mode"/>
    <xsl:call-template name="makeHTMLHeading">
      <xsl:with-param name="class">title</xsl:with-param>
      <xsl:with-param name="text">
        <xsl:call-template name="generateTitle"/>
      </xsl:with-param>
      <xsl:with-param name="level">1</xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="makeHTMLHeading">
      <xsl:with-param name="class">subtitle</xsl:with-param>
      <xsl:with-param name="text">
        <xsl:call-template name="generateSubTitle"/>
      </xsl:with-param>
      <xsl:with-param name="level">2</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <xsl:template name="stdheader">
    <xsl:param name="title" select="'(no title)'"/>
    <xsl:call-template name="pageHeader"/>
  </xsl:template>
  <xsl:template match="tei:titlePage">
    <!--
	<div class="titlePage">
	<h1>
	<xsl:apply-templates
	select="tei:docTitle/tei:titlePart/tei:title"/>
	</h1>
	<h2>
	<xsl:value-of select="tei:docAuthor"/>
	</h2>
	</div>
    -->
  </xsl:template>
  <xsl:template name="continuedToc">
    <xsl:if test="tei:div">
      <ul class="continuedtoc">
        <xsl:apply-templates mode="maketoc" select="tei:div"/>
      </ul>
    </xsl:if>
  </xsl:template>
  <xsl:template match="tei:div" mode="maketoc">
    <xsl:param name="forcedepth"/>
    <xsl:variable name="myName">
      <xsl:value-of select="local-name(.)"/>
    </xsl:variable>
    <xsl:if test="tei:head or $autoHead='true'">
      <xsl:variable name="Depth">
        <xsl:choose>
          <xsl:when test="not($forcedepth='')">
            <xsl:value-of select="$forcedepth"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="$tocDepth"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <xsl:variable name="thislevel">
        <xsl:value-of select="count(ancestor::tei:div)"/>
      </xsl:variable>
      <xsl:variable name="pointer">
        <xsl:apply-templates mode="generateLink" select="."/>
      </xsl:variable>
      <li>
        <xsl:choose>
	  <xsl:when test="$outputTarget='epub' or $outputTarget='epub3'"/>
          <xsl:when test="not(ancestor::tei:div) and tei:div">
            <xsl:attribute name="class">
              <xsl:text>tocTree</xsl:text>
            </xsl:attribute>
            <span class="showhide">
              <span class="tocShow">
                <img alt="+" src="Images/plus.gif"/>
              </span>
              <span class="tocHide">
                <img alt="-" src="Images/minus.gif"/>
              </span>
            </span>
          </xsl:when>
          <xsl:otherwise>
            <xsl:attribute name="class">
              <xsl:text>toc</xsl:text>
            </xsl:attribute>
            <span class="notshowhide">
              <xsl:text>&#160;&#160;</xsl:text>
            </span>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:call-template name="header">
          <xsl:with-param name="toc" select="$pointer"/>
          <xsl:with-param name="minimal">false</xsl:with-param>
          <xsl:with-param name="display">plain</xsl:with-param>
        </xsl:call-template>
        <xsl:if test="$thislevel &lt; $Depth">
          <xsl:call-template name="continuedToc"/>
        </xsl:if>
      </li>
    </xsl:if>
  </xsl:template>
  <!--
  <xsl:template name="mainTOC">
    <xsl:param name="force"/>

    <div class="toc_back">
      <h3>
      	  <xsl:call-template name="i18n">
	    <xsl:with-param name="word">Back Matter</xsl:with-param>
	  </xsl:call-template>
      </h3>
      <xsl:for-each
	  select="ancestor-or-self::tei:TEI/tei:text/tei:back">
	<xsl:if test="tei:div">
	  <ul class="toc{$force} toc_back">
	    <xsl:apply-templates mode="maketoc"
				 select="tei:div">
	      <xsl:with-param name="forcedepth" select="$force"/>
              </xsl:apply-templates>
	  </ul>
	</xsl:if>
      </xsl:for-each>
    </div>

    <div class="toc_front">
      <h3>
      	  <xsl:call-template name="i18n">
	    <xsl:with-param name="word">Front Matter</xsl:with-param>
	  </xsl:call-template>
      </h3>
      <xsl:for-each
	  select="ancestor-or-self::tei:TEI/tei:text/tei:front">
	<xsl:if	    test="tei:div">
	  <ul class="toc{$force} toc_front">
	    <xsl:apply-templates mode="maketoc" select="tei:div">
	      <xsl:with-param name="forcedepth" select="$force"/>
	    </xsl:apply-templates>
            </ul>
	</xsl:if>
      </xsl:for-each>
    </div>

    <div class="toc_body">
      <h3>
      	  <xsl:call-template name="i18n">
	    <xsl:with-param name="word">Text body</xsl:with-param>
	  </xsl:call-template>
      </h3>
      <xsl:for-each
        select="ancestor-or-self::tei:TEI/tei:text/tei:body">
        <xsl:if          test="tei:div">
          <ul class="toc{$force}  toc_body">
            <xsl:apply-templates mode="maketoc"
              select="tei:div">
              <xsl:with-param name="forcedepth" select="$force"/>
            </xsl:apply-templates>
          </ul>
        </xsl:if>
      </xsl:for-each>
    </div>

  </xsl:template>

-->
  <xsl:template match="tei:divGen[@type='toc']">
    <xsl:call-template name="mainPage"/>
  </xsl:template>
  <xsl:template name="javascriptHook">
    <script type="text/javascript" src="jquery-1.2.6.min.js">
      <xsl:comment>JQuery</xsl:comment>
    </script>
    <!--
    <script type="text/javascript" src="jquery.treeview.js">	
       <xsl:comment>JQuery treeview</xsl:comment>
    </script>
-->
    <script type="text/javascript" src="columnlist.js">
      <xsl:comment>JQuery columnlist</xsl:comment>
    </script>
    <script type="text/javascript">
        $(function() {
         $('ul.attrefs-class').columnizeList({cols:3,width:30,unit:'%'});
         $('ul.attrefs-element').columnizeList({cols:3,width:30,unit:'%'});
         $(".displayRelaxButton").click(function() {
           $(this).parent().find('.RNG_XML').toggle();
           $(this).parent().find('.RNG_Compact').toggle();
         });
         $(".tocTree .showhide").click(function() {
          $(this).find(".tocShow,.tocHide").toggle();
          $(this).parent().find("ul.continuedtoc").toggle();
	  });
        })
    </script>
    <xsl:if test="not($googleAnalytics='')">
      <script type="text/javascript" src="udm-all.js">
        <xsl:comment>UDM</xsl:comment>
      </script>
      <link rel="stylesheet" href="udm.css"/>
    </xsl:if>
    <xsl:call-template name="jsForOdds"/>
  </xsl:template>
  <xsl:template name="sectionHeadHook">
    <xsl:variable name="ident">
      <xsl:apply-templates mode="ident" select="."/>
    </xsl:variable>
    <xsl:variable name="d">
      <xsl:apply-templates mode="depth" select="."/>
    </xsl:variable>
    <xsl:if test="$d &gt; 0">
      <span class="bookmarklink">
        <a class="bookmarklink" href="#{$ident}">
          <xsl:attribute name="title">
            <xsl:text>link to this section </xsl:text>
          </xsl:attribute>
          <span class="invisible">
            <xsl:text>TEI: </xsl:text>
            <xsl:value-of select="tei:head[1]"/>
          </span>
          <span class="pilcrow">
            <xsl:text>¶</xsl:text>
          </span>
        </a>
      </span>
    </xsl:if>
  </xsl:template>
  <xsl:template match="/div"> </xsl:template>
  <xsl:template name="myi18n">
    <xsl:param name="word"/>
    <xsl:choose>
      <xsl:when test="$word='previousWord'">
        <span class="icon">
          <xsl:text>« </xsl:text>
        </span>
      </xsl:when>
      <xsl:when test="$word='upWord'">
        <span class="icon">
          <xsl:text>↑ </xsl:text>
        </span>
      </xsl:when>
      <xsl:when test="$word='nextWord'">
        <span class="icon">
          <xsl:text>» </xsl:text>
        </span>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <!-- only use listBibl if its in right language-->
  <xsl:template match="tei:listBibl[@xml:lang and not($documentationLanguage=@xml:lang)]"/>

  <!-- link from bibl back to egXML -->
  <xsl:template
      match="tei:listBibl/tei:biblStruct|tei:listBibl/tei:bibl">
    <xsl:apply-templates/>
    <xsl:for-each select="key('BACKLINKS',@xml:id)">
      <xsl:if test="self::teix:egXML">
        <xsl:text> </xsl:text>
        <a class="link_return" title="Go back to text">
          <xsl:attribute name="href">
            <xsl:apply-templates select="." mode="generateLink"/>
          </xsl:attribute>
          <xsl:text>↵</xsl:text>
        </a>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="egXMLEndHook">
    <xsl:if test="@corresp and id(substring(@corresp,2))">
      <div style="float: right;">
        <a>
          <xsl:attribute name="href">
            <xsl:apply-templates mode="generateLink" select="id(substring(@corresp,2))"/>
          </xsl:attribute>
          <xsl:text>bibliography</xsl:text>
          <!--	  <span class="citLink">&#x270d;</span>-->
        </a>
	<xsl:text>&#160;</xsl:text>
      </div>
    </xsl:if>
    <xsl:for-each select="ancestor::tei:elementSpec">
      <div style="float: right;">
        <a href="examples-{@ident}.html">
          <xsl:call-template name="i18n">
            <xsl:with-param name="word">Show all</xsl:with-param>
          </xsl:call-template>
        </a>
	<xsl:text>&#160;</xsl:text>
      </div>
    </xsl:for-each>
  </xsl:template>
  <xsl:template name="figureHook">
    <xsl:if test="@corresp and id(substring(@corresp,2))">
      <div style="float: right;">
        <a>
          <xsl:attribute name="href">
            <xsl:apply-templates mode="generateLink" select="id(substring(@corresp,2))"/>
          </xsl:attribute>
          <xsl:text>bibliography</xsl:text>
        </a>
      </div>
    </xsl:if>
  </xsl:template>
  <xsl:template name="stdfooter">
    <xsl:param name="style" select="'plain'"/>
    <xsl:param name="file">index</xsl:param>
    <xsl:variable name="date">
      <xsl:call-template name="generateDate"/>
    </xsl:variable>
    <xsl:variable name="author">
      <xsl:call-template name="generateAuthor"/>
    </xsl:variable>
    <div class="stdfooter">
      <xsl:if test="$outputTarget='html'">
        <p>
	  [<a href="../../en/html/{$file}.html">English</a>]
	  [<a href="../../de/html/{$file}.html">Deutsch</a>]
	  [<a href="../../es/html/{$file}.html">Español</a>]
	  [<a href="../../it/html/{$file}.html">Italiano</a>]
	  [<a href="../../fr/html/{$file}.html">Français</a>]
	  [<a href="../../ja/html/{$file}.html">日本語</a>]
	  [<a href="../../kr/html/{$file}.html">한국어</a>]
	  [<a href="../../zh-TW/html/{$file}.html">中文</a>]
	</p>
      </xsl:if>
      <hr/>
      <xsl:if test="$linkPanel='true'">
        <div class="footer">
          <xsl:if test="not($parentURL='')">
            <a class="{$style}" href="{$parentURL}">
              <xsl:value-of select="$parentWords"/>
            </a>
          </xsl:if>
          <xsl:if test="$searchURL"> | <a class="{$style}" href="{$searchURL}" target="_top"><xsl:call-template name="searchWords"/></a>
          </xsl:if>
          <xsl:if test="$feedbackURL"> | <a class="{$style}" href="{$feedbackURL}"><xsl:call-template name="feedbackWords"/></a>
          </xsl:if>
        </div>
        <hr/>
      </xsl:if>
      <xsl:for-each
	  select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:availability">
	<div class="availability">
	  <xsl:apply-templates/>
	  <xsl:choose>
	    <xsl:when test="count(tei:licence)&gt;1">
	      <ol>
		<xsl:for-each select="tei:licence">
		  <li>
		    <xsl:choose>
		      <xsl:when test="@target">
			<a href="{@target}"><xsl:value-of select="@target"/></a>
		      </xsl:when>
		      <xsl:otherwise>			
			<xsl:apply-templates/>
		      </xsl:otherwise>
		    </xsl:choose>
		  </li>
		</xsl:for-each>
	      </ol>
	    </xsl:when>
	    <xsl:otherwise>
	      <div class="licence">
		<xsl:for-each select="tei:licence">
		  <a href="{@target}">
		    <xsl:apply-templates/>
		  </a>
		</xsl:for-each>
	      </div>
	    </xsl:otherwise>
	  </xsl:choose>
	</div>
      </xsl:for-each>

      <address>
	<br/>
        <xsl:text>Version </xsl:text>
        <xsl:value-of 
	    select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/tei:edition"/>
        <xsl:text>This page generated on </xsl:text> 
	<xsl:call-template name="whatsTheDate"/>
      </address>
    </div>
    <xsl:if test="not($googleAnalytics='')">
      <script src="http://www.google-analytics.com/urchin.js" type="text/javascript">
        <!-- load google analytics -->
      </script>
      <script type="text/javascript">
        <xsl:text>_uacct = "</xsl:text>
        <xsl:value-of select="$googleAnalytics"/>
        <xsl:text>";
	urchinTracker();
	</xsl:text>
      </script>
    </xsl:if>
  </xsl:template>
  <xsl:template name="guidelinesTop">
    <xsl:param name="name"/>
    <xsl:if test="$outputTarget='html'">
      <xsl:if test="not($googleAnalytics='')">
        <script type="text/javascript" src="udm-dom.js">
          <xsl:comment>&#160;</xsl:comment>
        </script>
        <script type="text/javascript" src="udm-mod-keyboard.js">
          <xsl:comment>&#160;</xsl:comment>
        </script>
      </xsl:if>
      <div id="container">
        <a href="#contentstart" title="Skip to content" class="skip">skip to content</a>
        <div id="banner">
          <img src="Images/banner.jpg" alt="Text Encoding Initiative logo and banner"/>
        </div>
        <xsl:if test="not($googleAnalytics='')">
          <xsl:copy-of select="document('staticnav.xml')/html:ul"/>
        </xsl:if>
      </div>
      <div id="searchbox" style="float:left;">
        <form action="http://www.google.com/search" method="get">
          <fieldset><input style="color:#225588;" value="" maxlength="255" size="20" name="q" type="text"/>&#160;<select name="sitesearch"><option value="http://www.tei-c.org/">Entire site</option><option value="http://www.tei-c.org/release/doc/tei-p5-doc/{$documentationLanguage}/html/" selected="selected">P5 Guidelines
	    <xsl:choose><xsl:when test="$documentationLanguage='en'"> — English</xsl:when><xsl:when test="$documentationLanguage='de'"> — Deutsch</xsl:when><xsl:when test="$documentationLanguage='es'"> — Español</xsl:when><xsl:when test="$documentationLanguage='it'"> — Italiano</xsl:when><xsl:when test="$documentationLanguage='fr'"> — Français</xsl:when><xsl:when test="$documentationLanguage='ja'"> — 日本語</xsl:when><xsl:when test="$documentationLanguage='kr'"> — 한국어</xsl:when><xsl:when test="$documentationLanguage='zh-TW'"> — 中文</xsl:when></xsl:choose>
	    </option><option value="http://www.tei-c.org/Guidelines/P4/html/">P4 Guidelines</option></select>&#160;<input style="font-size:100%; font-weight:bold;      color:#FFFFFF; background-color:#225588; height: 2em;" value="Search" type="submit"/></fieldset>
        </form>
      </div>
    </xsl:if>
    <div class="mainhead">
      <h1>P5: 
    <xsl:call-template name="i18n"><xsl:with-param name="word">GuidelinesTEI</xsl:with-param></xsl:call-template>
    </h1>
    </div>
  </xsl:template>
  <xsl:template name="generateParentsByAttribute">
    <xsl:variable name="this" select="@ident"/>
    <xsl:if test="count(key('ATTREFS-CLASS',$this))&gt;0">
      <div>
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">
            <xsl:text>Class</xsl:text>
          </xsl:with-param>
        </xsl:call-template>
        <xsl:text>: </xsl:text>
        <ul class="attrefs-class">
          <xsl:for-each select="key('ATTREFS-CLASS',$this)">
            <xsl:sort select="ancestor::tei:classSpec/@ident"/>
            <xsl:sort select="@ident"/>
            <li>
              <xsl:for-each select="ancestor::tei:classSpec">
                <xsl:call-template name="linkTogether">
                  <xsl:with-param name="name">
                    <xsl:value-of select="@ident"/>
                  </xsl:with-param>
                  <xsl:with-param name="class">
                    <xsl:text>link_odd_class</xsl:text>
                  </xsl:with-param>
                </xsl:call-template>
              </xsl:for-each>
              <xsl:text>/@</xsl:text>
              <xsl:value-of select="ancestor::tei:attDef/@ident"/>
              <xsl:call-template name="showSpace"/>
            </li>
          </xsl:for-each>
        </ul>
      </div>
    </xsl:if>
    <xsl:if test="count(key('ATTREFS-ELEMENT',$this))&gt;0">
      <div>
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">
            <xsl:text>Element</xsl:text>
          </xsl:with-param>
        </xsl:call-template>
        <xsl:text>: </xsl:text>
        <ul class="attrefs-element">
          <xsl:for-each select="key('ATTREFS-ELEMENT',$this)">
            <xsl:sort select="ancestor::tei:elementSpec/@ident"/>
            <xsl:sort select="@ident"/>
            <li>
              <xsl:for-each select="ancestor::tei:elementSpec">
                <xsl:call-template name="linkTogether">
                  <xsl:with-param name="name">
                    <xsl:value-of select="@ident"/>
                  </xsl:with-param>
                  <xsl:with-param name="class">
                    <xsl:text>link_odd_element</xsl:text>
                  </xsl:with-param>
                </xsl:call-template>
              </xsl:for-each>
              <xsl:text>/@</xsl:text>
	      <xsl:for-each select="ancestor::tei:attDef">
		<xsl:value-of select="(tei:altIdent|@ident)[last()]"/>
	      </xsl:for-each>
              <xsl:call-template name="showSpace"/>
            </li>
          </xsl:for-each>
        </ul>
      </div>
    </xsl:if>
  </xsl:template>
</xsl:stylesheet>
