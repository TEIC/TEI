<?xml version="1.0" encoding="utf-8"?>
<!-- ===================================================================

This stylesheet is meant as a replacement to a DTD for XSL Formatting
objects. It checks structural validity of an XSL FO document, issuing
messages at inconsistencies found. As compared to DTD, stylesheet
validation offers the following advantages:

1. More flexibility in controlling the document structure. For example,
the stylesheet can control the following aspects of the document
structure that a DTD cannot handle:

  - check for block-level content through intermediate fo:wrapper elements;
  - exclude contents of fo:instream-foreign-object from validation;
  - check mutual exclusion of @internal-destination and @external-destination;
  - check fo:marker/fo:initial-property-set position in a mixed content model;
  - check that fo:marker is a descendant of fo:flow;
  - check that a footnote may not contain other footnotes or floats;
  - etc.

2. Improved readability of validation error messages. Frequent errors can get
a separate message that explains the problem and eventually offers a hint
to fix it. (In many respects, this is still a TODO :-)).

3. Namespace awareness. The namespace prefix need not be fixed anymore
(as it was the case with DTD validation).

4. Smart handling of attribute values - "partial" validation. This stylesheet
catches misprints in predefined attribute values but passes expressions through.

*************************************************************************

IMPLEMENTATION NOTES

1. Stylesheet structure and techniques.

The structure of this stylesheet is trivial: we apply templates to each elements
and attributes, and issue an error message each time there is something suspicious
about the document structure. There is a dozen of named templates that assist us
in performing common tasks.

1.1. Validating element names and content models

Each element described in XSL 1.0 has a matching template, and all
incorrectly named or missplaced elements are catched by a default rule
(redefined to complain about invalid elements). Inside each template,
there is a number of checks for obligatory attributes and content model.
The document-node template checks namespace correctness and presence
of fo:root as the top node.

1.2. Validating attribute occurrency

By design of XSL, almost every property may be in principle specified
anywhere. Attribute occurrence can be reliably limited only in the
following places:

  - fo:layout-master-set and its descendants;
  - terminal inline elements - fo:character, fo:page-number,
    fo:page-number-citation, fo:initial-property-set;
  - graphics - fo:external-graphics, fo:instream-foreign-object.

I used the following approach to achieve this: the default rule
for @* complains about incorrect attribute. However, the priority
for this default rule differs across the element tree:

 * -2 for fo:root, fo:page-sequence and all descendants
   of fo:page-sequence;

 *  2 for fo:simple-page-master and its descendants;

 *  4 for fo:layout-master-set and its descendants other than
      fo:simple-master/fo:region-*.

For each correct attribute, the stylesheet contains an enabling template.
Attributes that can occur only inside fo:page-sequence get enabling
templates with default priority; attributes permitted on region
descriptors get priority="3"; attributes of other fo:layout-master-set
children will have priority="5".

To limit attribute occurrence on atomic inlines and graphics, there are
two disabling named templates defined. They are added inside enabling
templates.

1.3. Validating attribute values.

Validation of attribute values is hard in XSL FO because of expressions:
even properties with a closed list of possible values can be specified
as expressions. This stylesheet validates attributes "partially":

 - attributes that can get only fixed values are validated only if their value
   does not contain a pair of parentheses (because any expression inside them
   should inevitably contain a function call);

 - attributes that can get numeric values are validated if their value
   does not contain any of the following:

     * parentheses;
     * math operators;
     * digits.

This gives a reliable method of catching mistyped values of attributes.

There are three cases where additional processing is necessary:

  A. Validation of URIs. Attributes that may get URI values are checked
     to be of the form 'url(...)'.

  B. Validation of 'content-type' attribute. It is checked to start
     with either 'content-type:' or 'namespace:'.

  C. Validation of 'text-align' attribute. This attribute may have an
     arbitrary string as its value; therefore, its correct validation
     is impossible - a mistype in any of the fixed values turns it
     into a 'string'. Because 'string' alignment is rare, and fixed
     values for text-align are ubiquitous, I felt necessary to introduce
     some heuristics to distinguish between table-alignment strings
     and mistyped predefined tokens. The heuristics is as follows:
     if a value contains only alphabetic characters, it's a keyword 
     and should match one of the predefined tokens. Otherwise, it's 
     a string and should not be spellchecked.

     (Frankly speaking, a 'string' value of "left" cannot be distinguished
     from a token 'left'. The property is really poorly designed.)

2. This stylesheet also contains checks for some RenderX extensions to XSL FO.
They are placed into a separate namespace, and should not interfere
with the correct validation of conformant XSL FO documents.

==================================================================== --><xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:fo="http://www.w3.org/1999/XSL/Format"
                xmlns:rx="http://www.renderx.com/XSL/Extensions"
                version="1.0"
                exclude-result-prefixes="fo rx">

   <xsl:output method="xml" omit-xml-declaration="yes"/>

   <!-- Strictness level. Currently used levels: 0 (loose), 1 (normal), 2 (pedantic) -->
<xsl:param name="strictness">1</xsl:param>

   <!-- =================================== -->
<!-- Match topmost node - must be <root> -->

<xsl:template match="/">
      <xsl:if test="not(fo:root)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">
               <xsl:choose>
                  <xsl:when test="local-name(*[1]) = 'root'">
                     <xsl:text>Incorrect namespace at top element - should be 'http://www.w3.org/1999/XSL/Format'</xsl:text>
                  </xsl:when>
                  <xsl:otherwise>
                     <xsl:text>Incorrect top element - should be 'root'</xsl:text>
                  </xsl:otherwise>
               </xsl:choose>
            </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:apply-templates select="fo:root"/>
   </xsl:template>


   <!-- =================================== -->
<!-- Root                                -->

<xsl:template match="fo:root">
      <xsl:if test="@*[.='inherit']">
         <xsl:call-template name="print-warning">
            <xsl:with-param name="msg">Attributes with value of 'inherit' are meaningless on '<xsl:value-of select="name()"/>'.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="no-text"/>

      <xsl:apply-templates select="*[not(self::rx:meta-info                                   or self::fo:layout-master-set                                   or self::fo:declarations                                   or self::rx:outline                                   or self::fo:page-sequence)]"
                           mode="report-intrusive-elements"/>

      <xsl:if test="not(fo:layout-master-set)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element 'layout-master-set' is required as a child of '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="count(fo:layout-master-set) &gt; 1">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">There can be only one '<xsl:value-of select="name(fo:layout-master-set[1])"/>' element as a child of '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="count(fo:declarations) &gt; 1">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">There can be only one '<xsl:value-of select="name(fo:declarations[1])"/>' element as a child of '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="not(fo:page-sequence)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">There should be at least one 'page-sequence' element as a child of '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="fo:layout-master-set[preceding-sibling::fo:declarations]">
         <xsl:call-template name="print-warning">
            <xsl:with-param name="msg">Element '<xsl:value-of select="name(fo:layout-master-set[1])"/>' shall precede '<xsl:value-of select="name(fo:declarations[1])"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="fo:declarations[preceding-sibling::fo:page-sequence]">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element '<xsl:value-of select="name(fo:declarations[1])"/>' shall be placed before the first '<xsl:value-of select="name(fo:page-sequence[1])"/>' element. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="fo:layout-master-set[preceding-sibling::fo:page-sequence]">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element '<xsl:value-of select="name(fo:layout-master-set[1])"/>' shall be placed before the first '<xsl:value-of select="name(fo:page-sequence[1])"/>' element. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <!-- Tests of RenderX extension elements -->
  <xsl:if test="count(rx:meta-info) &gt; 1">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">There can be only one '<xsl:value-of select="name(rx:meta-info[1])"/>' element as a child of '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="count(rx:outline) &gt; 1">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">There can be only one '<xsl:value-of select="name(rx:outline[1])"/>' element as a child of '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="rx:meta-info[preceding-sibling::*]">
         <xsl:call-template name="print-warning">
            <xsl:with-param name="msg">Element '<xsl:value-of select="name(rx:meta-info[1])"/>' should be the first child of '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="rx:outline[preceding-sibling::fo:page-sequence]">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element '<xsl:value-of select="name(rx:outline[1])"/>' shall be placed before the first '<xsl:value-of select="name(fo:page-sequence[1])"/>' element. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:apply-templates select="*"/>

   </xsl:template>


   <!-- =================================== -->
<!-- Layout-master-set                   -->

<xsl:template match="fo:layout-master-set">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="no-text"/>

      <xsl:apply-templates select="*[not(self::fo:simple-page-master                                   or self::fo:page-sequence-master)]"
                           mode="report-intrusive-elements">
         <xsl:with-param name="reason">Only 'simple-page-master' and 'page-sequence-master' elements are permitted in this context.</xsl:with-param>
      </xsl:apply-templates>

      <xsl:if test="not(fo:simple-page-master)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">There should be at least one 'simple-page-master' element inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:apply-templates select="*"/>
   </xsl:template>


   <!-- =================================== -->
<!-- Simple-page-master                  -->

<xsl:template match="fo:simple-page-master">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="no-text"/>

      <xsl:apply-templates select="*[not(self::fo:region-body                                   or self::fo:region-before                                   or self::fo:region-after                                   or self::fo:region-start                                   or self::fo:region-end)]"
                           mode="report-intrusive-elements">
         <xsl:with-param name="reason">Only region descriptors are permitted in this context.</xsl:with-param>
      </xsl:apply-templates>

      <xsl:if test="not(@master-name)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Attribute 'master-name' is required for '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:variable name="master-name" select="@master-name"/>
      <xsl:if test="ancestor::fo:layout-master-set             and (following-sibling::*[@master-name = $master-name]               or preceding-sibling::*[@master-name = $master-name])">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">
        Duplicate identifier: master-name="<xsl:value-of select="$master-name"/>".
        Property 'master-name' should be unique within '<xsl:value-of select="name(ancestor::fo:layout-master-set[1])"/>'.
      </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="not(fo:region-body)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element 'region-body' is required inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="count(fo:region-body) &gt; 1">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">There can be only one '<xsl:value-of select="name(fo:region-body[1])"/>' element inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="count(fo:region-before) &gt; 1">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">There can be only one '<xsl:value-of select="name(fo:region-before[1])"/>' element inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="count(fo:region-after) &gt; 1">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">There can be only one '<xsl:value-of select="name(fo:region-after[1])"/>' element inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="count(fo:region-start) &gt; 1">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">There can be only one '<xsl:value-of select="name(fo:region-start[1])"/>' element inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="count(fo:region-end) &gt; 1">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">There can be only one '<xsl:value-of select="name(fo:region-end[1])"/>' element inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <!-- I didn't want to control the ordering of regions; -->
  <!-- but they convinced me...                          -->

  <xsl:if test="$strictness &gt; 0">
         <xsl:if test="fo:region-body/preceding-sibling::*[self::fo:region-before                                                    or self::fo:region-after                                                    or self::fo:region-start                                                    or self::fo:region-end]                or fo:region-before/preceding-sibling::*[self::fo:region-after                                                    or self::fo:region-start                                                    or self::fo:region-end]                or fo:region-after/preceding-sibling::*[self::fo:region-start                                                    or self::fo:region-end]                or fo:region-start/preceding-sibling::fo:region-end">
            <xsl:call-template name="print-warning">
               <xsl:with-param name="msg">Incorrect order of region descriptors inside '<xsl:value-of select="name()"/>'. Regions shall be ordered according to the following content model: fo:region-body, fo:region-before?, fo:region-after?, fo:region-start?, fo:region-end?</xsl:with-param>
            </xsl:call-template>
         </xsl:if>
      </xsl:if>

      <xsl:apply-templates select="*"/>
   </xsl:template>

   <!-- =================================== -->
<!-- Regions                             -->

<xsl:template match="fo:region-body                    | fo:region-before                    | fo:region-after                    | fo:region-start                    | fo:region-end">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="empty-element"/>
      <xsl:if test="not(@extent) and not (self::fo:region-body)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Attribute 'extent' is required for '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:variable name="region-name">
         <xsl:choose>
            <xsl:when test="@region-name">
               <xsl:value-of select="@region-name"/>
            </xsl:when>
            <xsl:otherwise>
               <xsl:value-of select="concat('xsl-', local-name())"/>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:variable>

      <xsl:if test="ancestor::fo:simple-page-master              and (preceding-sibling::*[@region-name = $region-name]                or following-sibling::*[@region-name = $region-name])">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">
        Duplicate identifier: region-name="<xsl:value-of select="$region-name"/>".
        Property 'region-name' shall be unique within '<xsl:value-of select="name(ancestor::fo:simple-page-master[1])"/>'.
      </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
  
      <xsl:if test="$strictness &gt; 1">
         <xsl:if test="@*[starts-with(local-name(), 'padding')]                     [normalize-space(.) != '0'                   and normalize-space(.) != '0pt'                  and normalize-space(.) != '0px'                  and normalize-space(.) != '0mm'                  and normalize-space(.) != '0em'                  and normalize-space(.) != '0cm'                  and normalize-space(.) != '0pc'                  and normalize-space(.) != '0in']">
            <xsl:call-template name="print-warning">
               <xsl:with-param name="msg">
          Padding on '<xsl:value-of select="name()"/>' is a RenderX extension.
        </xsl:with-param>
            </xsl:call-template>
         </xsl:if>

         <xsl:if test="@*[starts-with(local-name(), 'border')]">
            <xsl:call-template name="print-warning">
               <xsl:with-param name="msg">
          Border on '<xsl:value-of select="name()"/>' is a RenderX extension.
       </xsl:with-param>
            </xsl:call-template>
         </xsl:if>
      </xsl:if>

   </xsl:template>


   <!-- =================================== -->
<!-- Page-sequence-master                -->

<xsl:template match="fo:page-sequence-master">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="no-text"/>

      <xsl:if test="not(@master-name)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Attribute 'master-name' is required for '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:variable name="master-name" select="@master-name"/>
      <xsl:if test="ancestor::fo:layout-master-set             and (following-sibling::*[@master-name = $master-name]               or preceding-sibling::*[@master-name = $master-name])">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">
        Duplicate identifier: master-name="<xsl:value-of select="$master-name"/>".
        Property 'master-name' should be unique within '<xsl:value-of select="name(ancestor::fo:layout-master-set[1])"/>'.
      </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:apply-templates select="*[not(self::fo:single-page-master-reference                                   or self::fo:repeatable-page-master-reference                                   or self::fo:repeatable-page-master-alternatives)]"
                           mode="report-intrusive-elements">
         <xsl:with-param name="reason">Only page sequence specifiers are permitted in this context.</xsl:with-param>
      </xsl:apply-templates>

      <xsl:if test="not (fo:single-page-master-reference                   or fo:repeatable-page-master-reference                   or fo:repeatable-page-master-alternatives)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">At least one page sequence specifier is required inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:apply-templates select="*"/>
   </xsl:template>


   <!-- =================================== -->
<!-- Page master references              -->

<xsl:template match="fo:single-page-master-reference                    | fo:repeatable-page-master-reference                    | fo:conditional-page-master-reference">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="empty-element"/>
      <xsl:if test="not(@master-reference)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Attribute 'master-reference' is required for '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
   </xsl:template>


   <!-- =================================== -->
<!-- Repeatable-page-master-alternatives -->

<xsl:template match="fo:repeatable-page-master-alternatives">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="no-text"/>

      <xsl:apply-templates select="*[not(self::fo:conditional-page-master-reference)]"
                           mode="report-intrusive-elements">
         <xsl:with-param name="reason">Only 'conditional-page-master-reference' elements are permitted in this context.</xsl:with-param>
      </xsl:apply-templates>

      <xsl:if test="not(fo:conditional-page-master-reference)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">At least one 'conditional-page-master-reference' element should be present inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:apply-templates select="*"/>
   </xsl:template>

   <!-- =================================== -->
<!-- Rx:meta-info                        -->

<xsl:template match="rx:meta-info">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="no-text"/>

      <xsl:apply-templates select="*[not(self::rx:meta-field)]" mode="report-intrusive-elements">
         <xsl:with-param name="reason">Only 'meta-field' elements are permitted in this context.</xsl:with-param>
      </xsl:apply-templates>

      <xsl:if test="not (rx:meta-field)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">At least one 'meta-field' element should be present inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:apply-templates select="*"/>
   </xsl:template>


   <!-- =================================== -->
<!-- Rx:meta-field                       -->

<xsl:template match="rx:meta-field">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="empty-element"/>
      <xsl:if test="not(@name)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Attribute 'name' is required for '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:if test="not(@value)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Attribute 'value' is required for '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
   </xsl:template>

   <!-- =================================== -->
<!-- Rx:page-device                      -->

<xsl:template match="rx:page-device" mode="report-intrusive-elements">
      <xsl:call-template name="print-error">
         <xsl:with-param name="msg">Element 'page-device' is obsolete;  use &lt;?xep-postscript-* ?&gt; processing instructions instead.</xsl:with-param>
      </xsl:call-template>
   </xsl:template>


   <!-- =================================== -->
<!-- Declarations                        -->

<xsl:template match="fo:declarations">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="no-text"/>

      <xsl:apply-templates select="*[not(self::fo:color-profile)]" mode="report-intrusive-elements">
         <xsl:with-param name="reason">Only 'color-profile' elements are permitted in this context.</xsl:with-param>
      </xsl:apply-templates>

      <xsl:apply-templates select="*"/>
   </xsl:template>


   <!-- =================================== -->
<!-- Color-profile                       -->

<xsl:template match="fo:color-profile">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="empty-element"/>
      <xsl:if test="not(@color-profile-name)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Attribute 'color-profile-name' is required for '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:if test="not(@src)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Attribute 'src' is required for '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
   </xsl:template>


   <!-- =================================== -->
<!-- Rx:outline                        -->

<xsl:template match="rx:outline">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="no-text"/>

      <xsl:apply-templates select="*[not(self::rx:bookmark)]" mode="report-intrusive-elements">
         <xsl:with-param name="reason">Only 'bookmark' elements are permitted in this context.</xsl:with-param>
      </xsl:apply-templates>

      <xsl:if test="not (rx:bookmark)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">At least one 'bookmark' element should be present inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:apply-templates select="*"/>
   </xsl:template>


   <!-- =================================== -->
<!-- Rx:bookmark                         -->

<xsl:template match="rx:bookmark">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="no-text"/>

      <xsl:if test="not(@internal-destination or @external-destination)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Either 'internal-destination' or 'external-destination' attribute must be present on '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="@internal-destination and @external-destination">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Only one of 'internal-destination' or 'external-destination' attributes can be present on '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:apply-templates select="*[not(self::rx:bookmark                                   or self::rx:bookmark-label)]"
                           mode="report-intrusive-elements">
         <xsl:with-param name="reason">Only 'bookmark-label' and/or nested 'boormark' elements are permitted in this context.</xsl:with-param>
      </xsl:apply-templates>

      <xsl:if test="not (rx:bookmark-label)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element 'bookmark-label' is required inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="count (rx:bookmark-label) &gt; 1">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">There should be exactly one '<xsl:value-of select="name(rx:bookmark-label[1])"/>' element inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="rx:bookmark-label[preceding-sibling::*]">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element '<xsl:value-of select="name(rx:bookmark-label[1])"/>' should be the first child of '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:apply-templates select="*"/>
   </xsl:template>


   <!-- =================================== -->
<!-- Rx:bookmark-label                   -->

<xsl:template match="rx:bookmark-label">
      <xsl:apply-templates select="@*"/>

      <xsl:apply-templates select="*" mode="report-intrusive-elements">
         <xsl:with-param name="reason">Only plain text is permitted in this context.</xsl:with-param>
      </xsl:apply-templates>
   </xsl:template>


   <!-- =================================== -->
<!-- Page-sequence                       -->

<xsl:template match="fo:page-sequence">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="no-text"/>

      <xsl:if test="not(@master-reference)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Attribute 'master-reference' is required for '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:apply-templates select="*[not(self::fo:title                                   or self::fo:static-content                                   or self::fo:flow)]"
                           mode="report-intrusive-elements">
         <xsl:with-param name="reason">Only 'flow', 'static-content', and 'title' elements are permitted in this context.</xsl:with-param>
      </xsl:apply-templates>

      <xsl:if test="not(fo:flow)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element 'flow' is required inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="count(fo:flow) &gt; 1">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">There can be only one '<xsl:value-of select="name(fo:flow[1])"/>' element inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="count(fo:title) &gt; 1">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">There can be at most one '<xsl:value-of select="name(fo:title[1])"/>' element inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="fo:title[preceding-sibling::*]">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element '<xsl:value-of select="name(fo:title[1])"/>' should be the first child of its parent '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="fo:flow[following-sibling::*]">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element '<xsl:value-of select="name(fo:flow[1])"/>' should be the last child of its parent '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:apply-templates select="*"/>
   </xsl:template>


   <!-- =================================== -->
<!-- Title                               -->

<xsl:template match="fo:title">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="inline-level"/>
      <xsl:apply-templates select="*"/>
   </xsl:template>


   <!-- =================================== -->
<!-- Flow and static-content             -->

<xsl:template match="fo:flow | fo:static-content">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="block-level"/>

      <xsl:if test="not(@flow-name)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Attribute 'flow-name' is required for '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:variable name="flow-name" select="@flow-name"/>
      <xsl:if test="ancestor::fo:page-sequence             and (following-sibling::*[self::fo:flow or self::fo:static-content][@flow-name = $flow-name]               or preceding-sibling::*[self::fo:flow or self::fo:static-content][@flow-name = $flow-name])">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">
        Duplicate identifier: flow-name="<xsl:value-of select="$flow-name"/>".
        Property 'flow-name' should be unique within '<xsl:value-of select="name(ancestor::fo:page-sequence[1])"/>'.
      </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:apply-templates select="*"/>
   </xsl:template>


   <!-- =================================== -->
<!-- Rx:flow-section                     -->

<xsl:template match="rx:flow-section">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="block-level"/>

      <xsl:if test="not(parent::fo:flow)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element '<xsl:value-of select="name()"/>' must be a direct descendant of 'flow'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:apply-templates select="*"/>
   </xsl:template>


   <!-- =================================== -->
<!-- Block                               -->

<xsl:template match="fo:block">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="mixed-level"/>
      <xsl:apply-templates select="*"/>
   </xsl:template>


   <!-- =================================== -->
<!-- Containers                          -->

<xsl:template match="fo:block-container | fo:inline-container">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="block-level"/>
      <xsl:apply-templates select="*"/>
   </xsl:template>


   <!-- =================================== -->
<!-- Bidi-override                       -->

<xsl:template match="fo:bidi-override">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="inline-level"/>

      <xsl:if test="not(@direction)">
         <xsl:call-template name="print-warning">
            <xsl:with-param name="msg">Attribute 'direction' is required for '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:apply-templates select="*"/>
   </xsl:template>


   <!-- =================================== -->
<!-- Character                           -->

<xsl:template match="fo:character">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="empty-element"/>

      <xsl:if test="not(@character)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Attribute 'character' is required for '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
   </xsl:template>


   <!-- =================================== -->
<!-- Initial-property-set                -->

<xsl:template match="fo:initial-property-set">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="empty-element"/>

      <xsl:choose>
         <xsl:when test="parent::fo:block">
            <xsl:if test="preceding-sibling::*">
               <xsl:call-template name="print-error">
                  <xsl:with-param name="msg">Element '<xsl:value-of select="name()"/>' should be the first child of '<xsl:value-of select="name(parent::fo:block)"/>'. </xsl:with-param>
               </xsl:call-template>
            </xsl:if>

            <xsl:if test="preceding-sibling::text()[normalize-space() != '']">
               <xsl:call-template name="print-error">
                  <xsl:with-param name="msg">Element '<xsl:value-of select="name()"/>' cannot be preceded by non-space characters in '<xsl:value-of select="name(parent::fo:block)"/>'. </xsl:with-param>
               </xsl:call-template>
            </xsl:if>

            <xsl:if test="preceding-sibling::fo:initial-property-set">
               <xsl:call-template name="print-error">
                  <xsl:with-param name="msg">Only one '<xsl:value-of select="name()"/>' element is permitted in 'xsl:value-of select="name(parent::fo:block)"/&gt;'. </xsl:with-param>
               </xsl:call-template>
            </xsl:if>
         </xsl:when>

         <xsl:otherwise>
            <xsl:call-template name="print-error">
               <xsl:with-param name="msg">Element '<xsl:value-of select="name()"/>' can only occur inside 'block'. </xsl:with-param>
            </xsl:call-template>
         </xsl:otherwise>
      </xsl:choose>
   </xsl:template>


   <!-- =================================== -->
<!-- Line-numerator                      -->

<xsl:template match="rx:ruler">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="empty-element"/>

      <xsl:choose>
         <xsl:when test="parent::fo:block-container">
            <xsl:if test="preceding-sibling::*">
               <xsl:call-template name="print-error">
                  <xsl:with-param name="msg">Element '<xsl:value-of select="name()"/>' should be the first child of '<xsl:value-of select="name(parent::fo:block-container)"/>'. </xsl:with-param>
               </xsl:call-template>
            </xsl:if>

            <xsl:if test="preceding-sibling::text()[normalize-space() != '']">
               <xsl:call-template name="print-error">
                  <xsl:with-param name="msg">Element '<xsl:value-of select="name()"/>' cannot be preceded by non-space characters in '<xsl:value-of select="name(parent::fo:block-container)"/>'. </xsl:with-param>
               </xsl:call-template>
            </xsl:if>

            <xsl:if test="preceding-sibling::rx:ruler">
               <xsl:call-template name="print-error">
                  <xsl:with-param name="msg">Only one '<xsl:value-of select="name()"/>' element is permitted in 'xsl:value-of select="name(parent::fo:block-container)"/&gt;'. </xsl:with-param>
               </xsl:call-template>
            </xsl:if>
         </xsl:when>

         <xsl:otherwise>
            <xsl:call-template name="print-error">
               <xsl:with-param name="msg">Element '<xsl:value-of select="name()"/>' can only occur inside 'block-container'. </xsl:with-param>
            </xsl:call-template>
         </xsl:otherwise>
      </xsl:choose>
   </xsl:template>


   <!-- =================================== -->
<!-- External-graphic                    -->

<xsl:template match="fo:external-graphic">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="empty-element"/>

      <xsl:if test="not(@src)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Attribute 'src' is required for '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
   </xsl:template>


   <!-- =================================== -->
<!-- Instream-foreign-object             -->

<xsl:template match="fo:instream-foreign-object">
      <xsl:apply-templates select="@*"/>
      <!-- only the attributes are checked -->
</xsl:template>


   <!-- =================================== -->
<!-- Inline                              -->

<xsl:template match="fo:inline">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="mixed-level"/>
      <xsl:apply-templates select="*"/>
   </xsl:template>


   <!-- =================================== -->
<!-- Leader                              -->

<xsl:template match="fo:leader">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="inline-level"/>
      <xsl:apply-templates select="*"/>
   </xsl:template>


   <!-- =================================== -->
<!-- Page-number                         -->

<xsl:template match="fo:page-number">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="empty-element"/>
   </xsl:template>


   <!-- =================================== -->
<!-- Page-number-citation                -->

<xsl:template match="fo:page-number-citation">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="empty-element"/>

      <xsl:if test="not(@ref-id)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Attribute 'ref-id' is required for '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
   </xsl:template>

   <!-- ========================================================== -->
<!-- Rx:page-number-citation-last (actually an XSL 1.1 element) -->

<xsl:template match="rx:page-number-citation-last">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="empty-element"/>

      <xsl:if test="not(@ref-id)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Attribute 'ref-id' is required for '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
   </xsl:template>


   <!-- =================================== -->
<!-- Rx:pinpoint                        -->

<xsl:template match="rx:pinpoint">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="empty-element"/>

      <xsl:if test="not(@value)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Attribute 'value' is required for '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
   </xsl:template>


   <!-- =================================== -->
<!-- Rx:page-index                       -->

<xsl:template match="rx:page-index">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="no-text"/>

      <xsl:apply-templates select="*[not(self::rx:index-item)]" mode="report-intrusive-elements">
         <xsl:with-param name="reason">Only 'rx:index-item' elements are permitted in this context</xsl:with-param>
      </xsl:apply-templates>

      <xsl:if test="not(@ref-key) and not (rx:index-item)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element '<xsl:value-of select="name()"/>' must have either 'ref-key' attribute, or 'rx:index-item' descendant elements.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:if test="@ref-key and rx:index-item">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element '<xsl:value-of select="name()"/>' can have either 'ref-key' attribute or '<xsl:value-of select="name(rx:index-item[1])"/>' descendant elements, but not both.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:apply-templates select="*"/>
   </xsl:template>

   <!-- =================================== -->
<!-- Rx:index-item                       -->

<xsl:template match="rx:index-item">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="empty-element"/>

      <xsl:if test="not(@ref-key)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Attribute 'ref-key' is required for '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
   </xsl:template>


   <!-- =================================== -->
<!-- Rx:begin-index-range                -->

<xsl:template match="rx:begin-index-range">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="empty-element"/>

      <xsl:if test="not(@rx:key)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Attribute 'rx:key' is required for '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
  
      <xsl:choose>
         <xsl:when test="not(@id)">
            <xsl:call-template name="print-error">
               <xsl:with-param name="msg">Attribute 'id' is required for '<xsl:value-of select="name()"/>'. </xsl:with-param>
            </xsl:call-template>
         </xsl:when>
         <xsl:otherwise>
            <xsl:variable name="id" select="@id"/>
      
            <xsl:if test="count(preceding::rx:begin-index-range[@id=$id]) = 1">
	              <xsl:call-template name="print-error">
	                 <xsl:with-param name="msg">Two or more '<xsl:value-of select="name()"/>' elements with id='<xsl:value-of select="@id"/>';  'id' should be unique within the document.</xsl:with-param>
	              </xsl:call-template>
            </xsl:if>
      
            <xsl:choose> 
               <xsl:when test="following::rx:end-index-range[@ref-id=$id]"/>
               <xsl:when test="preceding::rx:end-index-range[@ref-id=$id]"/>
               <xsl:otherwise>
                  <xsl:call-template name="print-error">
                     <xsl:with-param name="msg">Unmatched '<xsl:value-of select="name()"/>' element with id='<xsl:value-of select="@id"/>': no correspondent 'rx:end-index-range' found. </xsl:with-param>
                  </xsl:call-template>
               </xsl:otherwise>
            </xsl:choose> 
         </xsl:otherwise>
      </xsl:choose>
   </xsl:template>

   <!-- =================================== -->
<!-- Rx:end-index-range                  -->

<xsl:template match="rx:end-index-range">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="empty-element"/>

      <xsl:choose>
         <xsl:when test="not(@ref-id)">
            <xsl:call-template name="print-error">
               <xsl:with-param name="msg">Attribute 'ref-id' is required for '<xsl:value-of select="name()"/>'. </xsl:with-param>
            </xsl:call-template>
         </xsl:when>
         <xsl:otherwise>
            <xsl:variable name="refid" select="@ref-id"/>

            <xsl:if test="count(preceding::rx:end-index-range[@ref-id=$refid]) = 1">
               <xsl:call-template name="print-error">
                  <xsl:with-param name="msg">Two or more '<xsl:value-of select="name()"/>' elements with ref-id='<xsl:value-of select="@ref-id"/>'.</xsl:with-param>
               </xsl:call-template>
            </xsl:if>

            <xsl:choose> 
               <xsl:when test="following::rx:begin-index-range[@id=$refid]">
                  <xsl:call-template name="print-warning">
                     <xsl:with-param name="msg">Element '<xsl:value-of select="name()"/>' with ref-id='<xsl:value-of select="@ref-id"/>' precedes its matching '<xsl:value-of select="name(following::rx:begin-index-range[@id=$refid][1])"/>'. </xsl:with-param>
                  </xsl:call-template>
               </xsl:when>
               <xsl:when test="preceding::rx:begin-index-range[@id=$refid]"/>
               <xsl:otherwise>
                  <xsl:call-template name="print-error">
                     <xsl:with-param name="msg">Unmatched '<xsl:value-of select="name()"/>' element with ref-id='<xsl:value-of select="@ref-id"/>': no correspondent 'rx:begin-index-range' found. </xsl:with-param>
                  </xsl:call-template>
               </xsl:otherwise>
            </xsl:choose> 
         </xsl:otherwise>
      </xsl:choose>
   </xsl:template>


   <!-- =================================== -->
<!-- Table-and-caption                   -->

<xsl:template match="fo:table-and-caption">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="no-text"/>

      <xsl:apply-templates select="*[not(self::fo:table-caption                                   or self::fo:table                                   or self::fo:marker)]"
                           mode="report-intrusive-elements">
         <xsl:with-param name="reason">Only 'table-caption' and 'table' elements are permitted in this context.</xsl:with-param>
      </xsl:apply-templates>

      <xsl:if test="not(fo:table)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element 'table' is required inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="count(fo:table) &gt; 1">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">There can be only one '<xsl:value-of select="name(fo:table[1])"/>' element inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="count(fo:table-caption) &gt; 1">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">There can be only one '<xsl:value-of select="name(fo:table-caption[1])"/>' element inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="fo:table-caption[preceding-sibling::fo:table]">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element '<xsl:value-of select="name(fo:table-caption[1])"/>' must precede '<xsl:value-of select="name(fo:table[1])"/>' inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:apply-templates select="*"/>
   </xsl:template>


   <!-- =================================== -->
<!-- Table-caption                       -->

<xsl:template match="fo:table-caption">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="block-level"/>
      <xsl:apply-templates select="*"/>
   </xsl:template>


   <!-- =================================== -->
<!-- Table                               -->

<xsl:template match="fo:table">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="no-text"/>

      <xsl:apply-templates select="*[not(self::fo:table-column                                   or self::fo:table-header                                   or self::fo:table-footer                                   or self::fo:table-body                                   or self::fo:marker)]"
                           mode="report-intrusive-elements">
         <xsl:with-param name="reason">Only 'table-column' elements, row group elements, and markers are permitted in this context.</xsl:with-param>
      </xsl:apply-templates>

      <xsl:if test="not(fo:table-body)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">At least one 'table-body' is required inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="count(fo:table-header) &gt; 1">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">There can be only one '<xsl:value-of select="name(fo:table-header[1])"/>' element inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="count(fo:table-footer) &gt; 1">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">There can be only one '<xsl:value-of select="name(fo:table-footer[1])"/>' element inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="fo:table-column[preceding-sibling::*[not(self::fo:table-column or self::fo:marker)]]">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">'<xsl:value-of select="name(fo:table-column[1])"/>' elements should be located before any other element in '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="fo:table-header[preceding-sibling::fo:table-footer]">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element '<xsl:value-of select="name(fo:table-header[1])"/>' should be located before '<xsl:value-of select="name(fo:table-footer[1])"/>' in '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="fo:table-header[preceding-sibling::fo:table-body]">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element '<xsl:value-of select="name(fo:table-header[1])"/>' should be located before '<xsl:value-of select="name(fo:table-body[1])"/>' in '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="fo:table-body[following-sibling::*[not(self::fo:table-body)]]">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">'<xsl:value-of select="name(fo:table-body[1])"/>' elements should be located last in '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:apply-templates select="*"/>
   </xsl:template>


   <!-- =================================== -->
<!-- Row group elements                  -->

<xsl:template match="fo:table-body | fo:table-header | fo:table-footer">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="no-text"/>

      <xsl:apply-templates select="*[not(self::fo:table-row                                   or self::fo:table-cell                                   or self::fo:marker)]"
                           mode="report-intrusive-elements">
         <xsl:with-param name="reason">Only 'table-row', 'table-cell', or 'marker' elements are permitted in this context.</xsl:with-param>
      </xsl:apply-templates>

      <xsl:if test="$strictness &gt; 0">
         <xsl:if test="not (*)">
            <xsl:call-template name="print-error">
               <xsl:with-param name="msg">Empty '<xsl:value-of select="name()"/>'. </xsl:with-param>
            </xsl:call-template>
         </xsl:if>
      </xsl:if>

      <xsl:if test="fo:table-row and fo:table-cell">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">'<xsl:value-of select="name(fo:table-row[1])"/>' and '<xsl:value-of select="name(fo:table-cell[1])"/>' elements should not be mixed inside '<xsl:value-of select="name()"/>' element. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:apply-templates select="*"/>
   </xsl:template>


   <!-- =================================== -->
<!-- Table-row                           -->

<xsl:template match="fo:table-row">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="no-text"/>

      <xsl:apply-templates select="*[not(self::fo:table-cell)]" mode="report-intrusive-elements">
         <xsl:with-param name="reason">Only 'table-cell' elements are permitted in this context.</xsl:with-param>
      </xsl:apply-templates>

      <xsl:if test="not(parent::fo:table-body or parent::fo:table-header or parent::fo:table-footer)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element '<xsl:value-of select="name()"/>' should be wrapped in 'table-body', 'table-header', or 'table-footer'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:apply-templates select="*"/>
   </xsl:template>

   <!-- =================================== -->
<!-- Table-cell                          -->

<xsl:template match="fo:table-cell">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="block-level"/>
      <xsl:apply-templates select="*"/>
   </xsl:template>


   <!-- =================================== -->
<!-- List-block                          -->

<xsl:template match="fo:list-block">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="no-text"/>

      <xsl:apply-templates select="*[not(self::fo:list-item or self::fo:marker)]"
                           mode="report-intrusive-elements">
         <xsl:with-param name="reason">Only 'list-item' or 'marker' elements are permitted in this context.</xsl:with-param>
      </xsl:apply-templates>

      <xsl:apply-templates select="*"/>
   </xsl:template>


   <!-- =================================== -->
<!-- List-item                           -->

<xsl:template match="fo:list-item">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="no-text"/>

      <xsl:apply-templates select="*[not(self::fo:list-item-label                                   or self::fo:list-item-body                                   or self::fo:marker)]"
                           mode="report-intrusive-elements">
         <xsl:with-param name="reason">Element '<xsl:value-of select="name()"/>' shall consist of one 'list-item-label' and one 'list-item-body'.</xsl:with-param>
      </xsl:apply-templates>

      <xsl:if test="not(parent::fo:list-block)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element '<xsl:value-of select="name()"/>' should be a child of 'list-block'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="not(fo:list-item-label)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element 'list-item-label' is required inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="not(fo:list-item-body)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element 'list-item-body' is required inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="count(fo:list-item-label) &gt; 1">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">There can be only one '<xsl:value-of select="name(fo:list-item-label[1])"/>' element inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="count(fo:list-item-body) &gt; 1">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">There can be only one '<xsl:value-of select="name(fo:list-item-body[1])"/>' element inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="fo:list-item-body[following-sibling::fo:list-item-label]">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element '<xsl:value-of select="name(fo:list-item-label[1])"/>' must precede '<xsl:value-of select="name(fo:list-item-body[1])"/>' inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="fo:list-item-label and not (fo:list-item-label/@start-indent)                                    and not (fo:list-item-label/@margin)                                    and not (fo:list-item-label/@margin-left)                                    and not (fo:list-item-label/@margin-right)                                    and not (fo:list-item-label/@margin-top)                                    and not (fo:list-item-label/@margin-bottom)              and fo:list-item-body and not (fo:list-item-body/@start-indent)                                    and not (fo:list-item-body/@margin)                                    and not (fo:list-item-body/@margin-left)                                    and not (fo:list-item-body/@margin-right)                                    and not (fo:list-item-body/@margin-top)                                    and not (fo:list-item-body/@margin-bottom)">
         <xsl:call-template name="print-warning">
            <xsl:with-param name="msg">Start-indent is not specified neither on '<xsl:value-of select="name(fo:list-item-label[1])"/>' nor on '<xsl:value-of select="name(fo:list-item-body[1])"/>': list subcomponents will overlap</xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="fo:list-item-label and not (fo:list-item-label/@end-indent)                                    and not (fo:list-item-label/@margin)                                    and not (fo:list-item-label/@margin-left)                                    and not (fo:list-item-label/@margin-right)                                    and not (fo:list-item-label/@margin-top)                                    and not (fo:list-item-label/@margin-bottom)              and fo:list-item-body and not (fo:list-item-body/@end-indent)                                    and not (fo:list-item-body/@margin)                                    and not (fo:list-item-body/@margin-left)                                    and not (fo:list-item-body/@margin-right)                                    and not (fo:list-item-body/@margin-top)                                    and not (fo:list-item-body/@margin-bottom)">
         <xsl:call-template name="print-warning">
            <xsl:with-param name="msg">End-indent is not specified neither on '<xsl:value-of select="name(fo:list-item-label[1])"/>' nor on '<xsl:value-of select="name(fo:list-item-body[1])"/>': list subcomponents will overlap</xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:apply-templates select="*"/>
   </xsl:template>


   <!-- =================================== -->
<!-- List item subcomponents             -->

<xsl:template match="fo:list-item-label | fo:list-item-body">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="block-level"/>
      <xsl:apply-templates select="*"/>
   </xsl:template>


   <!-- =================================== -->
<!-- Footnote                            -->

<xsl:template match="fo:footnote">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="no-text"/>
      <xsl:call-template name="out-of-line"/>

      <xsl:apply-templates select="*[not(self::fo:inline                                   or self::fo:footnote-body)]"
                           mode="report-intrusive-elements">
         <xsl:with-param name="reason">Element '<xsl:value-of select="name()"/>' shall consist of one 'inline' and one 'footnote-body'.</xsl:with-param>
      </xsl:apply-templates>

      <xsl:if test="not(fo:inline)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element 'inline' is required inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="not(fo:footnote-body)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element 'footnote-body' is required inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="count(fo:inline) &gt; 1">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">There can be only one '<xsl:value-of select="name(fo:inline[1])"/>' element inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="count(fo:footnote-body) &gt; 1">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">There can be only one '<xsl:value-of select="name(fo:footnote-body[1])"/>' element inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="fo:footnote-body[following-sibling::fo:inline]">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element '<xsl:value-of select="name(fo:inline[1])"/>' must precede '<xsl:value-of select="name(fo:footnote-body[1])"/>' inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:for-each select="fo:inline">
         <xsl:call-template name="inline-level"/>
      </xsl:for-each>

      <xsl:apply-templates select="*"/>
   </xsl:template>


   <!-- =================================== -->
<!-- Footnote-body                       -->

<xsl:template match="fo:footnote-body">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="block-level"/>
      <xsl:apply-templates select="*"/>
   </xsl:template>


   <!-- =================================== -->
<!-- Float                               -->

<xsl:template match="fo:float">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="block-level"/>
      <xsl:call-template name="out-of-line"/>
      <xsl:apply-templates select="*"/>
   </xsl:template>


   <!-- =================================== -->
<!-- Wrapper                             -->

<xsl:template match="fo:wrapper">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="mixed-level"/>
      <xsl:apply-templates select="*"/>
   </xsl:template>


   <!-- === =============================== -->
<!-- Marker                              -->

<xsl:template match="fo:marker">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="mixed-level"/>

      <xsl:choose>
         <xsl:when test="not(@marker-class-name)">
            <xsl:call-template name="print-error">
               <xsl:with-param name="msg">Attribute 'marker-class-name' is required for '<xsl:value-of select="name()"/>'. </xsl:with-param>
            </xsl:call-template>
         </xsl:when>
         <xsl:otherwise>
            <xsl:variable name="marker-class-name" select="@marker-class-name"/>
            <xsl:if test="preceding-sibling::fo:marker[@marker-class-name=$marker-class-name]">
               <xsl:call-template name="print-error">
                  <xsl:with-param name="msg">Sibling <xsl:value-of select="name()"/> elements have the same value of 'marker-class-name' attribute ("<xsl:value-of select="@marker-class-name"/>"). </xsl:with-param>
               </xsl:call-template>
            </xsl:if>
         </xsl:otherwise>
      </xsl:choose>

      <xsl:if test="preceding-sibling::*[not(self::fo:marker)]">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">'<xsl:value-of select="name()"/>' elements should be initial children of their parents. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="preceding-sibling::text()[normalize-space() != '']">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element '<xsl:value-of select="name()"/>' cannot be preceded by non-space text nodes. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="not(ancestor::fo:flow)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element '<xsl:value-of select="name()"/>' should be a descendant of 'flow'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="ancestor::fo:multi-case">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element '<xsl:value-of select="name()"/>' cannot be used inside '<xsl:value-of select="name(ancestor::fo:multi-case[1])"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="ancestor::fo:float">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element '<xsl:value-of select="name()"/>' cannot be used inside '<xsl:value-of select="name(ancestor::fo:float[1])"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="ancestor::fo:footnote">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element '<xsl:value-of select="name()"/>' cannot be used inside '<xsl:value-of select="name(ancestor::fo:footnote[1])"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:apply-templates select="*"/>
   </xsl:template>

   <!-- === =============================== -->
<!-- Retrieve-marker                     -->

<xsl:template match="fo:retrieve-marker">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="empty-element"/>

      <xsl:if test="not(@retrieve-class-name)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Attribute 'retrieve-class-name' is required for '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="not(ancestor::fo:static-content)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element '<xsl:value-of select="name()"/>' should be a descendant of 'static-content'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
   </xsl:template>


   <!-- === =============================== -->
<!-- Basic-link                          -->

<xsl:template match="fo:basic-link">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="mixed-level"/>

      <xsl:if test="not(@internal-destination or @external-destination)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Either 'internal-destination' or 'external-destination' attribute must be present on '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="@internal-destination and @external-destination">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Only one of 'internal-destination' or 'external-destination' attributes can be present on '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:apply-templates select="*"/>
   </xsl:template>


   <!-- =================================== -->
<!-- Multi-switch                        -->

<xsl:template match="fo:multi-switch">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="no-text"/>

      <xsl:apply-templates select="*[not(self::fo:multi-case)]" mode="report-intrusive-elements">
         <xsl:with-param name="reason">Only 'multi-case' elements are permitted in this context.</xsl:with-param>
      </xsl:apply-templates>

      <xsl:apply-templates select="*"/>
   </xsl:template>


   <!-- =================================== -->
<!-- Multi-case                          -->

<xsl:template match="fo:multi-case">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="mixed-level"/>
      <xsl:if test="not(parent::fo:multi-switch)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element '<xsl:value-of select="name()"/>' should be a child of 'multi-switch'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:apply-templates select="*"/>
   </xsl:template>


   <!-- ===================================== -->
<!-- Multi-toggle                          -->

<xsl:template match="fo:multi-toggle">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="mixed-level"/>

      <xsl:if test="not(ancestor::fo:multi-case)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element '<xsl:value-of select="name()"/>' can be used only inside 'multi-case'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:apply-templates select="*"/>
   </xsl:template>


   <!-- ===================================== -->
<!-- Multi-properties                    -->

<xsl:template match="fo:multi-properties">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="no-text"/>

      <xsl:apply-templates select="*[not(self::fo:multi-property-set                                   or self::fo:wrapper)]"
                           mode="report-intrusive-elements">
         <xsl:with-param name="reason">Element '<xsl:value-of select="name()"/>' shall consist of one or more 'multi-property-set' elements, followed by a 'wrapper'.</xsl:with-param>
      </xsl:apply-templates>

      <xsl:if test="not(fo:multi-property-set)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">At least one 'multi-property-set' element should be specified in '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="not(fo:wrapper)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element 'wrapper' is required inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="count(fo:wrapper) &gt; 1">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">There can be only one '<xsl:value-of select="name(fo:wrapper[1])"/>' element inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="fo:wrapper[following-sibling::fo:multi-property-set]">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element '<xsl:value-of select="name(fo:wrapper[1])"/>' must be located after all '<xsl:value-of select="name(fo:multi-property-set[1])"/>' elements inside '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:apply-templates select="*"/>
   </xsl:template>


   <!-- ===================================== -->
<!-- Multi-property-set                    -->

<xsl:template match="fo:multi-property-set">
      <xsl:apply-templates select="@*"/>
      <xsl:call-template name="empty-element"/>

      <xsl:if test="not(@active-state)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Attribute 'active-state' is required for '<xsl:value-of select="name()"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="not(parent::fo:multi-properties)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element '<xsl:value-of select="name()"/>' should be a child of 'multi-properties'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

   </xsl:template>


   <!-- =================================== -->
<!-- Common structure-checking templates -->

<!-- Check for block-level elements         -->

<xsl:template name="block-level">
      <xsl:call-template name="no-text">
         <xsl:with-param name="reason">Only block-level elements are permitted in this context.</xsl:with-param>
      </xsl:call-template>

      <xsl:apply-templates select="*[not(self::fo:block                                   or self::fo:block-container                                   or self::fo:list-block                                   or self::fo:table                                   or self::fo:table-and-caption                                   or self::fo:float                                   or self::fo:wrapper                                   or self::fo:marker                                   or self::fo:retrieve-marker                                   or self::fo:multi-switch                                   or self::fo:multi-properties                                   or self::rx:ruler                                   or self::rx:begin-index-range                                   or self::rx:end-index-range                                   or self::rx:flow-section)]"
                           mode="report-intrusive-elements-block">
         <xsl:with-param name="reason">Only block-level elements are permitted in this context.</xsl:with-param>
      </xsl:apply-templates>
      <xsl:for-each select="fo:wrapper | fo:multi-switch/fo:multi-case | fo:multi-properties">
         <xsl:call-template name="block-level"/>
      </xsl:for-each>
   </xsl:template>


   <!-- Check for inline-level elements         -->

<xsl:template name="inline-level">
      <xsl:apply-templates select="*[not(self::fo:bidi-override                                   or self::fo:character                                   or self::fo:external-graphic                                   or self::fo:instream-foreign-object                                   or self::fo:inline                                   or self::fo:inline-container                                   or self::fo:leader                                   or self::fo:page-number                                   or self::fo:page-number-citation                                   or self::fo:basic-link                                   or self::fo:multi-toggle                                   or self::fo:footnote                                   or self::fo:float                                   or self::fo:wrapper                                   or self::fo:marker                                   or self::fo:retrieve-marker                                   or self::fo:multi-switch                                   or self::fo:multi-properties                                  or self::rx:page-index                                   or self::rx:page-number-citation-last                                   or self::rx:page-index                                   or self::rx:begin-index-range                                   or self::rx:end-index-range                                   or self::rx:pinpoint)]"
                           mode="report-intrusive-elements-inline">
         <xsl:with-param name="reason">Only inline-level elements are permitted in this context.</xsl:with-param>
      </xsl:apply-templates>
      <xsl:for-each select="fo:wrapper | fo:multi-switch/fo:multi-case | fo:multi-properties | fo:multi-toggle | fo:inline | fo:basic-link | fo:bidi-override">
         <xsl:call-template name="inline-level"/>
      </xsl:for-each>
   </xsl:template>


   <!-- Check for mixed-level elements         -->

<xsl:template name="mixed-level">
      <xsl:apply-templates select="*[not(self::fo:block                                   or self::fo:block-container                                   or self::fo:list-block                                   or self::fo:table                                   or self::fo:table-and-caption                                   or self::fo:bidi-override                                   or self::fo:character                                   or self::fo:external-graphic                                   or self::fo:initial-property-set                                   or self::fo:instream-foreign-object                                   or self::fo:inline                                   or self::fo:inline-container                                   or self::fo:leader                                   or self::fo:page-number                                   or self::fo:page-number-citation                                   or self::fo:basic-link                                   or self::fo:multi-toggle                                   or self::fo:footnote                                   or self::fo:float                                   or self::fo:wrapper                                   or self::fo:marker                                   or self::fo:retrieve-marker                                   or self::fo:multi-switch                                   or self::fo:multi-properties                                   or self::rx:page-number-citation-last                                   or self::rx:page-index                                   or self::rx:begin-index-range                                   or self::rx:end-index-range                                   or self::rx:pinpoint)]"
                           mode="report-intrusive-elements-block">
         <xsl:with-param name="reason">Only block-level or inline-level elements are permitted in this context.</xsl:with-param>
      </xsl:apply-templates>
      <xsl:for-each select="fo:wrapper | fo:multi-switch/fo:multi-case | fo:multi-properties">
         <xsl:call-template name="mixed-level"/>
      </xsl:for-each>
   </xsl:template>


   <!-- Check constraints applicable to out-of-line elements  -->

<xsl:template name="out-of-line">
      <xsl:if test="not(ancestor::fo:flow)">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element '<xsl:value-of select="name()"/>' should be a descendant of 'flow'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="ancestor::fo:float">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element '<xsl:value-of select="name()"/>' cannot be a descendant of '<xsl:value-of select="name(ancestor::fo:float[1])"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="ancestor::fo:footnote-body">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element '<xsl:value-of select="name()"/>' cannot be a descendant of '<xsl:value-of select="name(ancestor::fo:footnote-body[1])"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="ancestor::fo:marker">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element '<xsl:value-of select="name()"/>' cannot be a descendant of '<xsl:value-of select="name(ancestor::fo:marker[1])"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>

      <xsl:if test="ancestor::*[@position='absolute' or @absolute-position='absolute'                          or @position='fixed' or @absolute-position='fixed']">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element '<xsl:value-of select="name()"/>' cannot be a descendant of an absolutely positioned
      '<xsl:value-of select="name(ancestor::*[@position='absolute' or @absolute-position='absolute'                                            or @position='fixed' or @absolute-position='fixed'][1])"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
   </xsl:template>

   <!-- =================================== -->
<!-- Auxiliary templates                 -->


<!-- Check for non-blank #PCDATA -->

<xsl:template name="no-text">
      <xsl:param name="reason" select="''"/>

      <xsl:if test="text()[normalize-space(.) != '']">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">
        Element '<xsl:value-of select="name()"/>' cannot have
	non-empty text descendants here. <xsl:value-of select="text()[normalize-space(.) != '']"/>
               <xsl:if test="$reason != ''">
                  <xsl:value-of select="$reason"/>
               </xsl:if>
            </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
   </xsl:template>


   <!-- Check for element being EMPTY -->

<xsl:template name="empty-element">
      <xsl:if test="* | text()">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Element '<xsl:value-of select="name()"/>' must be empty. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
   </xsl:template>



   <!-- Report an element that may not be present in a given point -->

<xsl:template match="*" mode="report-intrusive-elements" priority="-1">
      <xsl:param name="reason" select="''"/>
      <xsl:variable name="uri" select="namespace-uri()"/>

      <xsl:choose>
         <xsl:when test="$uri='http://www.w3.org/1999/XSL/Format'                  or $uri='http://www.renderx.com/XSL/Extensions'">
            <xsl:call-template name="print-error">
               <xsl:with-param name="msg">
           Element '<xsl:value-of select="name()"/>' cannot be a child of '<xsl:value-of select="name(..)"/>'.
           <xsl:if test="$reason != ''">
                     <xsl:value-of select="$reason"/>
                  </xsl:if>
               </xsl:with-param>
            </xsl:call-template>
         </xsl:when>
         <xsl:when test="$uri = ''">
            <xsl:call-template name="print-error">
               <xsl:with-param name="msg">
          Element '<xsl:value-of select="name()"/>' belonging to the anonymous namespace cannot occur in an XSL-FO document outside 'instream-foreign-object' elements.
        </xsl:with-param>
            </xsl:call-template>
         </xsl:when>
         <xsl:when test="$uri = 'http://www.w3.org/2000/svg'">
            <xsl:if test="$strictness &gt; 0">
               <xsl:call-template name="print-warning">
                  <xsl:with-param name="msg">
            SVG element '<xsl:value-of select="name()"/>' is located outside 'instream-foreign-object' element.
          </xsl:with-param>
               </xsl:call-template>
            </xsl:if>
         </xsl:when>
         <xsl:otherwise>
            <xsl:if test="$strictness &gt; 1">
               <xsl:call-template name="print-warning">
                  <xsl:with-param name="msg">
             Element '<xsl:value-of select="name()"/>' belongs to an unknown namespace <xsl:value-of select="$uri"/>
                  </xsl:with-param>
               </xsl:call-template>
            </xsl:if>
         </xsl:otherwise>
      </xsl:choose>
   </xsl:template>

   <xsl:template match="*" mode="report-intrusive-elements-block" priority="-1">
      <xsl:param name="reason"/>

      <xsl:apply-templates select="." mode="report-intrusive-elements">
         <xsl:with-param name="reason" select="$reason"/>
      </xsl:apply-templates>
   </xsl:template>

   <xsl:template match="*" mode="report-intrusive-elements-inline" priority="-1">
      <xsl:param name="reason"/>

      <xsl:apply-templates select="." mode="report-intrusive-elements">
         <xsl:with-param name="reason" select="$reason"/>
      </xsl:apply-templates>
   </xsl:template>


   <!-- Special case: report an invalid element contained in a wrapper -->

<xsl:template match="fo:wrapper/*                    | fo:multi-properties/*                    | fo:multi-case/*"
                 mode="report-intrusive-elements-block">
      <xsl:param name="reason" select="''"/>

      <xsl:call-template name="print-error">
         <xsl:with-param name="msg">
      Element '<xsl:value-of select="name()"/>' cannot be a descendant of '<xsl:value-of select="name(ancestor::*[not(self::fo:wrapper or self::fo:multi-switch or self::fo:multi-case or self::fo:multi-properties)][1])"/>' through wrapper elements.
      <xsl:if test="$reason != ''">
               <xsl:value-of select="$reason"/>
            </xsl:if>
         </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="fo:wrapper/*                    | fo:inline/*                    | fo:basic-link/*                    | fo:bidi-override/*                    | fo:multi-toggle/*                    | fo:multi-properties/*                    | fo:multi-case/*"
                 mode="report-intrusive-elements-inline">
      <xsl:param name="reason" select="''"/>

      <xsl:call-template name="print-error">
         <xsl:with-param name="msg">
      Element '<xsl:value-of select="name()"/>' cannot be a descendant of '<xsl:value-of select="name(ancestor::*[not(self::fo:wrapper or self::fo:multi-switch or self::fo:multi-case or self::fo:multi-toggle or self::fo:multi-properties or self::fo:inline)][1])"/>' through wrapper or inline elements.
      <xsl:if test="$reason != ''">
               <xsl:value-of select="$reason"/>
            </xsl:if>
         </xsl:with-param>
      </xsl:call-template>
   </xsl:template>


   <!-- Default rules -->

<xsl:template match="*" priority="-1"/>
   <xsl:template match="text()"/>


   <!-- ##################################################################### -->
<!-- Processing of attributes.                                             -->
<!-- ##################################################################### -->

<!-- Report any attribute not covered by enabling templates -->

<xsl:template match="@*" priority="-2">
      <xsl:call-template name="complain-attribute"/>
   </xsl:template>

   <!-- Report any attribute in fo:layout-master-set etc. as invalid.         -->
<!-- This is overridden by a rule with higher priority for those (few)     -->
<!-- properties that actually may occur in these special positions         -->

<xsl:template match="@*[not (parent::fo:root)                     and not (ancestor::fo:page-sequence)                     and not (parent::*/parent::fo:simple-page-master)]"
                 priority="4">
      <xsl:call-template name="complain-attribute"/>
   </xsl:template>

   <xsl:template match="fo:simple-page-master/*/@*" priority="2">
      <xsl:call-template name="complain-attribute"/>
   </xsl:template>

   <xsl:template name="complain-attribute">
      <xsl:variable name="uri" select="namespace-uri()"/>

      <xsl:choose>
    <!-- Ignore attributes from XML namespace - xml:space, xml:base, etc. -->
    <xsl:when test="$uri='http://www.w3.org/XML/1998/namespace'"/> 
         <xsl:when test="$uri=''                  or $uri='http://www.w3.org/1999/XSL/Format'                  or $uri='http://www.renderx.com/XSL/Extensions'">
            <xsl:call-template name="print-error">
               <xsl:with-param name="msg">Attribute '<xsl:value-of select="name()"/>' cannot occur at element '<xsl:value-of select="name(..)"/>'. </xsl:with-param>
            </xsl:call-template>
         </xsl:when>
         <xsl:otherwise>
            <xsl:if test="$strictness &gt; 1">
               <xsl:call-template name="print-warning">
                  <xsl:with-param name="msg">Attribute '<xsl:value-of select="name()"/>' at element '<xsl:value-of select="name(..)"/>' belongs to an unknown namespace <xsl:value-of select="$uri"/> 
                  </xsl:with-param>
               </xsl:call-template>
            </xsl:if>
         </xsl:otherwise>
      </xsl:choose>
   </xsl:template>

   <xsl:template name="disable-on-atomic-inlines">
      <xsl:if test="parent::fo:character              or parent::fo:page-number              or parent::fo:page-number-citation              or parent::rx:page-number-citation-last              or parent::fo:initial-property-set">
         <xsl:call-template name="complain-attribute"/>
      </xsl:if>
   </xsl:template>


   <xsl:template name="disable-on-graphics">
      <xsl:if test="parent::fo:external-graphic              or parent::fo:instream-foreign-object">
         <xsl:call-template name="complain-attribute"/>
      </xsl:if>
   </xsl:template>

   <!-- Report a value not in a list. Expressions (recognized by the presence -->
<!-- of parentheses or operators) are excluded from validation.            -->

<xsl:template name="enumerated-values">
      <xsl:param name="valuelist"/>
      <xsl:variable name="value" select="normalize-space(.)"/>
      <xsl:choose>
         <xsl:when test="contains($valuelist, concat(' ', $value, ' '))"/>
         <xsl:when test="contains($value, '(') and contains ($value, ')')"/>
         <xsl:otherwise>
            <xsl:call-template name="print-error">
               <xsl:with-param name="msg">Attribute '<xsl:value-of select="name()"/>' cannot have a value of "<xsl:value-of select="$value"/>". </xsl:with-param>
            </xsl:call-template>
         </xsl:otherwise>
      </xsl:choose>
   </xsl:template>

   <!-- Report a value that is not a valid number or length. Skips validation -->
<!-- of attributes containing digits, parentheses, and math operators.     -->

<xsl:template name="quantitative-values">
      <xsl:param name="valuelist"/>
      <xsl:variable name="value" select="normalize-space(.)"/>
      <xsl:choose>
         <xsl:when test="contains($valuelist, concat(' ', $value, ' '))"/>
         <xsl:when test="contains($value, '(') and contains ($value, ')')"/>
         <xsl:when test="contains($value, '0')"/>
         <xsl:when test="contains($value, '1')"/>
         <xsl:when test="contains($value, '2')"/>
         <xsl:when test="contains($value, '3')"/>
         <xsl:when test="contains($value, '4')"/>
         <xsl:when test="contains($value, '5')"/>
         <xsl:when test="contains($value, '6')"/>
         <xsl:when test="contains($value, '7')"/>
         <xsl:when test="contains($value, '8')"/>
         <xsl:when test="contains($value, '9')"/>
         <xsl:when test="contains($value, '+')"/>
         <xsl:when test="contains($value, ' - ')"/>
         <xsl:when test="contains($value, '*')"/>
         <xsl:when test="contains($value, ' div ')"/>
         <xsl:when test="contains($value, ' mod ')"/>
         <xsl:otherwise>
            <xsl:call-template name="print-error">
               <xsl:with-param name="msg">Attribute '<xsl:value-of select="name()"/>' cannot have a value of "<xsl:value-of select="$value"/>". </xsl:with-param>
            </xsl:call-template>
         </xsl:otherwise>
      </xsl:choose>
   </xsl:template>

   <!-- Check if a value is valid as a URL specifier -->
<xsl:template name="check-url">
      <xsl:variable name="val" select="normalize-space()"/>
      <xsl:choose>
         <xsl:when test="$val = 'inherit'"/>
         <xsl:when test="starts-with($val, 'url(')                 and substring ($val, string-length($val)) = ')'"/>
         <xsl:otherwise>
            <xsl:call-template name="print-warning">
               <xsl:with-param name="msg">Attribute '<xsl:value-of select="name()"/>' cannot have a value of "<xsl:value-of select="."/>": should be either 'inherit' or a URI: <xsl:value-of select="name()"/>="url(...)".</xsl:with-param>
            </xsl:call-template>
         </xsl:otherwise>
      </xsl:choose>
   </xsl:template>

   <!-- 7.3. Accessibility properties - ubiquitous; not validated -->
<xsl:template match="@source-document" priority="5">
      <xsl:variable name="val" select="normalize-space()"/>
      <xsl:choose>
         <xsl:when test="$val = 'inherit'"/>
         <xsl:when test="$val = 'none'"/>
         <xsl:when test="starts-with($val, 'url(')                 and substring ($val, string-length($val)) = ')'"/>
         <xsl:otherwise>
            <xsl:call-template name="print-error">
               <xsl:with-param name="msg">Attribute '<xsl:value-of select="name()"/>' cannot have a value of "<xsl:value-of select="."/>": should be 'inherit', 'none', or one or more URIs: source="url(...) [url(...) ...]".</xsl:with-param>
            </xsl:call-template>
         </xsl:otherwise>
      </xsl:choose>
   </xsl:template>


   <xsl:template match="@role" priority="5"/>

   <!-- 7.4. Absolute position -->
<xsl:template match="fo:block-container/@absolute-position">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> auto absolute fixed inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@top | @bottom | @left | @right">
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> auto inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <!-- 7.5. Aural properties. May happen everywhere in the flows. Not validated. -->
<xsl:template match="@azimuth"/>
   <xsl:template match="@cue-after"/>
   <xsl:template match="@cue-before"/>

   <xsl:template match="@elevation">
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> below level above higher lower inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@pause-after | @pause-before">
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@pitch">
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> x-low low medium high x-high inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@pitch-range">
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@play-during"/>

   <xsl:template match="@richness">
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@speak">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> normal none spell-out inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@speak-header">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> once always inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@speak-numeral">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> digits continuous inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@speak-punctuation">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> code none inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@speech-rate">
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> x-slow slow medium fast x-fast faster slower inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@stress">
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@voice-family"/>
   <xsl:template match="@volume">
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> silent x-soft soft medium loud x-loud inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>


   <!-- 7.6. Border, padding, and background -->
<xsl:template priority="3" match="@background-attachment">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> scroll fixed inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template priority="3" match="@background-color"/>

   <xsl:template priority="3" match="@background-image">
      <xsl:if test=". != 'none'">
         <xsl:call-template name="check-url"/>
      </xsl:if>
   </xsl:template>

   <xsl:template priority="3" match="@background-repeat">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> repeat repeat-x repeat-y no-repeat inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template priority="3" match="@background-position-horizontal">
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> left center right inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template priority="3" match="@background-position-vertical">
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> top center bottom inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template priority="3"
                 match="@border-before-color                    | @border-after-color                    | @border-start-color                    | @border-end-color                    | @border-top-color                    | @border-bottom-color                    | @border-left-color                    | @border-right-color"/>

   <xsl:template priority="3"
                 match="@border-before-style                    | @border-after-style                    | @border-start-style                    | @border-end-style                    | @border-top-style                    | @border-bottom-style                    | @border-left-style                    | @border-right-style">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> none hidden dotted dashed solid double groove ridge inset outset inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template priority="3"
                 match="@border-before-width                    | @border-before-width.length                    | @border-after-width                    | @border-after-width.length                    | @border-start-width                    | @border-start-width.length                    | @border-end-width                    | @border-end-width.length                    | @border-top-width                    | @border-bottom-width                    | @border-left-width                    | @border-right-width">
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> thin medium thick inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@border-before-width.conditionality                    | @border-after-width.conditionality                    | @border-start-width.conditionality                    | @border-end-width.conditionality">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> discard retain </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template priority="3"
                 match="@padding-before                    | @padding-before.length                    | @padding-after                    | @padding-after.length                    | @padding-start                    | @padding-start.length                    | @padding-end                    | @padding-end.length                    | @padding-top                    | @padding-bottom                    | @padding-left                    | @padding-right">
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@padding-before.conditionality                    | @padding-after.conditionality                    | @padding-start.conditionality                    | @padding-end.conditionality">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> discard retain </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <!-- 7.7. Font properties  -->
<xsl:template match="@font-family">
      <xsl:call-template name="disable-on-graphics"/>
   </xsl:template>

   <xsl:template match="@font-selection-strategy">
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> auto character-by-character inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@font-size">
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> xx-small x-small small medium large x-large xx-large larger smaller inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@font-stretch">
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="quantitative-values"> <!-- this is kinda sorta extension -->
    <xsl:with-param name="valuelist"> normal wider narrower ultra-condensed extra-condensed condensed semi-condensed semi-expanded expanded extra-expanded ultra-expanded inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@font-size-adjust">
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> none inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@font-style">
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> normal italic oblique backslant inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@font-variant">
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> normal small-caps inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@font-weight">
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> normal bold bolder lighter inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <!-- 7.8. Hyphenation  -->
<xsl:template match="@country">
      <xsl:call-template name="disable-on-graphics"/>
   </xsl:template>

   <xsl:template match="@language">
      <xsl:call-template name="disable-on-graphics"/>
   </xsl:template>

   <xsl:template match="@script">
      <xsl:call-template name="disable-on-graphics"/>
   </xsl:template>

   <xsl:template match="@hyphenate">
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> false true inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@hyphenation-character">
      <xsl:call-template name="disable-on-graphics"/>
   </xsl:template>

   <xsl:template match="@hyphenation-push-character-count                    | @hyphenation-remain-character-count">
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>


   <!-- 7.9 - 7.10. Margin properties - block and inline  -->
<xsl:template match="@margin-top                    | @margin-bottom                    | @margin-left                    | @margin-right">
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> auto inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template priority="5"
                 match="fo:region-body/@margin-top                    | fo:region-body/@margin-bottom                    | fo:region-body/@margin-left                    | fo:region-body/@margin-right                    | fo:simple-page-master/@margin-top                    | fo:simple-page-master/@margin-bottom                    | fo:simple-page-master/@margin-left                    | fo:simple-page-master/@margin-right">
      <xsl:call-template name="quantitative-values"/>
   </xsl:template>

   <xsl:template match="@space-before                    | @space-before.minimum                    | @space-before.optimum                    | @space-before.maximum                    | @space-after                    | @space-after.minimum                    | @space-after.optimum                    | @space-after.maximum                    | @space-start                    | @space-start.minimum                    | @space-start.optimum                    | @space-start.maximum                    | @space-end                    | @space-end.minimum                    | @space-end.optimum                    | @space-end.maximum">
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>


   <xsl:template match="@space-before.conditionality                    | @space-after.conditionality                    | @space-start.conditionality                    | @space-end.conditionality">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> discard retain </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@space-before.precedence                    | @space-after.precedence                    | @space-start.precedence                    | @space-end.precedence">
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> force </xsl:with-param>
      </xsl:call-template>
   </xsl:template>


   <xsl:template match="@start-indent | @end-indent">
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>


   <!-- 7.11. Relative position  -->
<xsl:template match="@relative-position">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> static relative inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <!-- 7.12. Area alignment  -->
<xsl:template match="@alignment-adjust">
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> auto baseline before-edge text-before-edge middle central after-edge text-after-edge ideographic alphabetic hanging mathematical top bottom text-top text-bottom inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@alignment-baseline">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> auto baseline before-edge text-before-edge middle central after-edge text-after-edge ideographic alphabetic hanging mathematical top bottom text-top text-bottom inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@baseline-shift">
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> baseline sub super inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template priority="3" match="@display-align">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> auto before center after inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@dominant-baseline">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> auto use-script no-change reset-size ideographic alphabetic hanging mathematical central middle text-after-edge text-before-edge inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@relative-align">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> before baseline inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <!-- 7.13. Area dimensions  -->
<xsl:template match="@block-progression-dimension                    | @block-progression-dimension.minimum                    | @block-progression-dimension.optimum                    | @block-progression-dimension.maximum                    | @inline-progression-dimension                    | @inline-progression-dimension.minimum                    | @inline-progression-dimension.optimum                    | @inline-progression-dimension.maximum                    | @height                    | @width">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> auto inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@max-height | @max-width">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> none inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@min-height | @min-width">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@content-height | @content-width">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> auto scale-to-fit inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@scaling">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> uniform non-uniform inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@scaling-method">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> auto integer-pixels resample-any-method inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>


   <!-- 7.14. Block and line-related properties  -->
<xsl:template match="@hyphenation-keep">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> auto column page inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@hyphenation-ladder-count">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> no-limit inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@text-indent | @last-line-end-indent">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@line-height">
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> normal inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@line-height.minimum                    | @line-height.optimum                    | @line-height.maximum">
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@line-height.conditionality">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> discard retain </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@line-height.precedence">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> force </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@line-height-shift-adjustment">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> consider-shifts disregard-shifts inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@line-stacking-strategy">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> line-height font-height max-height inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@linefeed-treatment">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> ignore preserve treat-as-space treat-as-zero-width-space inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@white-space-treatment">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> ignore preserve ignore-if-before-linefeed ignore-if-after-linefeed ignore-if-surrounding-linefeed inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@text-align">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:variable name="keyword-charset"
                    select="'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'"/>
      <xsl:variable name="token" select="normalize-space()"/>
      <xsl:if test="string-length($token) != 0 and                 string-length(translate($token, $keyword-charset, '')) = 0">
         <xsl:call-template name="enumerated-values">
            <xsl:with-param name="valuelist"> start center end justify inside outside left right inherit </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
   </xsl:template>


   <xsl:template match="@text-align-last">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> relative start center end justify inside outside left right inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@white-space-collapse">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> false true inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@wrap-option">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> no-wrap wrap inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>


   <!-- 7.15. Character properties -->
<xsl:template match="fo:character/@character"/>

   <xsl:template match="@letter-spacing.minimum                     | @letter-spacing.optimum                      | @letter-spacing.maximum                      | @letter-spacing                     | @word-spacing.minimum                     | @word-spacing.optimum                     | @word-spacing.maximum                     | @word-spacing">
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> normal inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@word-spacing.conditionality                    | @letter-spacing.conditionality">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> discard retain </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@word-spacing.precedence                    | @letter-spacing.precedence">
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> force </xsl:with-param>
      </xsl:call-template>
   </xsl:template>


   <xsl:template match="@suppress-at-line-break">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> auto suppress retain inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@text-decoration">
      <xsl:call-template name="disable-on-graphics"/>
   </xsl:template>

   <xsl:template match="@text-shadow">
      <xsl:call-template name="disable-on-graphics"/>
   </xsl:template>

   <xsl:template match="@text-transform">
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> capitalize uppercase lowercase none inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@treat-as-word-space">
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> auto false true inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <!-- 7.16. Color properties -->
<xsl:template match="@color"/>

   <xsl:template match="fo:color-profile/@color-profile-name" priority="5"/>

   <xsl:template match="fo:color-profile/@rendering-intent                    | fo:declarations/@rendering-intent                    | fo:root/@rendering-intent"
                 priority="5">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> auto perceptual relative-colorimetric saturation absolute-colorimetric inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>


   <!-- 7.17. Float properties -->
<xsl:template match="@clear">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> start end left right both none inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@float">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> before start end left right inside outside none inherit </xsl:with-param>
      </xsl:call-template>

      <xsl:if test="$strictness &gt; 1">
         <xsl:if test=".='inside' or .='outside'">
            <xsl:call-template name="print-warning">
               <xsl:with-param name="msg">Value "<xsl:value-of select="."/>" for 'float' attribute is a RenderX extension.</xsl:with-param>
            </xsl:call-template>  
         </xsl:if>
      </xsl:if>
   </xsl:template>

   <xsl:template match="@intrusion-displace">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> auto none line indent block inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <!-- 7.18. Keeps and breaks -->
<xsl:template match="@break-after | @break-before">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> auto column page even-page odd-page inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@keep-together                    | @keep-together.within-line                    | @keep-together.within-column                    | @keep-together.within-page                    | @keep-with-next                    | @keep-with-next.within-line                    | @keep-with-next.within-column                    | @keep-with-next.within-page                    | @keep-with-previous                    | @keep-with-previous.within-line                    | @keep-with-previous.within-column                    | @keep-with-previous.within-page">
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> auto always inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@orphans | @widows">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>


   <!-- 7.19. Layout-related properties -->
<xsl:template priority="3" match="@clip"/>

   <xsl:template priority="3" match="@overflow">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> visible hidden scroll error-if-overflow auto inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template priority="3" match="@reference-orientation">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> 0 90 180 270 -90 -180 -270 inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template priority="5" match="fo:simple-page-master/@reference-orientation">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> 0 90 180 270 -90 -180 -270 inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@span">
      <xsl:if test="ancestor::fo:static-content">
         <xsl:call-template name="print-error">
            <xsl:with-param name="msg">Attribute 'span' cannot be used inside '<xsl:value-of select="name(ancestor::fo:static-content[1])"/>'. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> none all inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <!-- 7.20. Leader properties -->
<xsl:template match="@leader-alignment">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> none reference-area page inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@leader-pattern">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> space rule dots use-content inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@leader-pattern-width">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> use-font-metrics inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@leader-length                    | @leader-length.minimum                    | @leader-length.optimum                    | @leader-length.maximum">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@rule-style">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> none dotted dashed solid double groove ridge inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@rule-thickness">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <!-- 7.21. Dynamic effects and hyperrefs -->
<xsl:template match="fo:multi-property-set/@active-state">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> link visited active hover focus </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@auto-restore">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> false true </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="fo:multi-case/@case-name"/>
   <xsl:template match="fo:multi-case/@case-title"/>

   <xsl:template match="fo:basic-link/@destination-placement-offset">
      <xsl:call-template name="quantitative-values"/>
   </xsl:template>

   <xsl:template match="fo:basic-link/@external-destination">
      <xsl:call-template name="check-url"/>
   </xsl:template>
   <xsl:template match="rx:bookmark/@external-destination" priority="5">
      <xsl:call-template name="check-url"/>
   </xsl:template>

   <xsl:template match="fo:basic-link/@internal-destination"/>
   <xsl:template match="rx:bookmark/@internal-destination" priority="5"/>

   <xsl:template match="rx:bookmark/@collapse-subtree" priority="5">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> true false </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="fo:root/@rx:initial-destination"/>

   <xsl:template match="fo:basic-link/@indicate-destination">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> true false </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="fo:basic-link/@show-destination">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> replace new </xsl:with-param>
      </xsl:call-template>
   </xsl:template>
   <xsl:template match="rx:bookmark/@show-destination" priority="5">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> replace new </xsl:with-param>
      </xsl:call-template>
   </xsl:template>


   <xsl:template match="fo:multi-case/@starting-state">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> show hide </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="fo:multi-toggle/@switch-to"/>
   <xsl:template match="fo:basic-link/@target-presentation-context"/>
   <xsl:template match="fo:basic-link/@target-processing-context"/>
   <xsl:template match="fo:basic-link/@target-stylesheet"/>

   <!-- 7.22. Marker properties -->
<xsl:template match="fo:marker/@marker-class-name"/>
   <xsl:template match="fo:retrieve-marker/@retrieve-class-name"/>

   <xsl:template match="fo:retrieve-marker/@retrieve-position">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> first-starting-within-page first-including-carryover last-starting-within-page last-ending-within-page </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="fo:retrieve-marker/@retrieve-boundary">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> page page-sequence document </xsl:with-param>
      </xsl:call-template>
   </xsl:template>


   <!-- 7.23. Number format properties -->
<xsl:template match="fo:page-sequence/@format | rx:ruler/@format"/>
   <xsl:template match="fo:page-sequence/@grouping-separator | rx:ruler/@grouping-separator"/>

   <xsl:template match="fo:page-sequence/@grouping-size | rx:ruler/@grouping-size">
      <xsl:call-template name="quantitative-values"/>
   </xsl:template>

   <xsl:template match="fo:page-sequence/@letter-value | rx:ruler/@letter-value">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> auto alphabetic traditional </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <!-- 7.24. Pagination -->
<xsl:template priority="5" match="fo:conditional-page-master-reference/@blank-or-not-blank">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> blank not-blank any </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template priority="5" match="fo:conditional-page-master-reference/@odd-or-even">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> even odd any </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template priority="5" match="fo:conditional-page-master-reference/@page-position">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> first last rest any </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template priority="5" match="fo:region-body/@column-count">
      <xsl:call-template name="quantitative-values"/>
   </xsl:template>

   <xsl:template priority="5" match="fo:region-body/@column-gap">
      <xsl:call-template name="quantitative-values"/>
   </xsl:template>

   <xsl:template match="rx:flow-section/@column-count">
      <xsl:call-template name="quantitative-values"/>
   </xsl:template>

   <xsl:template match="rx:flow-section/@column-gap">
      <xsl:call-template name="quantitative-values"/>
   </xsl:template>

   <xsl:template priority="5"
                 match="fo:region-before/@extent                    | fo:region-after/@extent                    | fo:region-start/@extent                    | fo:region-end/@extent">
      <xsl:call-template name="quantitative-values"/>
   </xsl:template>

   <xsl:template match="fo:flow/@flow-name | fo:static-content/@flow-name"/>

   <xsl:template match="fo:page-sequence/@force-page-count">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> auto even odd end-on-even end-on-odd no-force </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="fo:page-sequence/@initial-page-number">
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> auto auto-odd auto-even </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template priority="5"
                 match="fo:simple-page-master/@master-name                    | fo:page-sequence-master/@master-name"/>

   <xsl:template priority="5"
                 match="fo:page-sequence/@master-reference                    | fo:single-page-master-reference/@master-reference                    | fo:repeatable-page-master-reference/@master-reference                    | fo:conditional-page-master-reference/@master-reference"/>

   <xsl:template priority="5"
                 match="fo:repeatable-page-master-reference/@maximum-repeats                    | fo:repeatable-page-master-alternatives/@maximum-repeats">
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> no-limit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>


   <xsl:template match="fo:root/@media-usage">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> auto paginate bounded-in-one-dimension unbounded </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template priority="5"
                 match="fo:simple-page-master/@page-height                    | fo:simple-page-master/@page-width">
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> auto indefinite </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template priority="5"
                 match="fo:region-before/@precedence                    | fo:region-after/@precedence                    | fo:region-start/@precedence                    | fo:region-end/@precedence">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> true false </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template priority="5"
                 match="fo:region-body/@region-name                    | fo:region-before/@region-name                    | fo:region-after/@region-name                    | fo:region-start/@region-name                    | fo:region-end/@region-name"/>

   <!-- 7.25. Table properties -->
<xsl:template match="@border-after-precedence                    | @border-before-precedence                    | @border-end-precedence                    | @border-start-precedence">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> force inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@border-collapse">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> collapse collapse-with-precedence separate inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@border-separation                    | @border-separation.block-progression-direction                    | @border-separation.inline-progression-direction">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>


   <xsl:template match="@caption-side">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> before after start end top bottom left right inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="fo:table-column/@column-number                    | fo:table-cell/@column-number">
      <xsl:call-template name="quantitative-values"/>
   </xsl:template>

   <xsl:template match="fo:table-column/@column-width">
      <xsl:call-template name="quantitative-values"/>
   </xsl:template>

   <xsl:template match="@empty-cells">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> show hide inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="fo:table-cell/@ends-row                    | fo:table-cell/@starts-row">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> true false </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="fo:table-column/@number-columns-repeated">
      <xsl:call-template name="quantitative-values"/>
   </xsl:template>

   <xsl:template match="fo:table-column/@number-columns-spanned                    | fo:table-cell/@number-columns-spanned">
      <xsl:call-template name="quantitative-values"/>
   </xsl:template>

   <xsl:template match="fo:table-cell/@number-rows-spanned">
      <xsl:call-template name="quantitative-values"/>
   </xsl:template>

   <xsl:template match="@table-layout">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> auto fixed inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="fo:table/@table-omit-footer-at-break                    | fo:table/@table-omit-header-at-break                     | fo:table/@rx:table-omit-initial-header                    | fo:table/@rx:table-omit-final-footer">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> true false </xsl:with-param>
      </xsl:call-template>
   </xsl:template>


   <!-- 7.26. Writing mode -->
<xsl:template match="@direction">
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> ltr rtl inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@glyph-orientation-horizontal">
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@glyph-orientation-vertical">
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> auto inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@text-altitude | @text-depth">
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> use-font-metrics inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>


   <xsl:template match="@unicode-bidi">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> normal embed bidi-override inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template priority="3" match="@writing-mode">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> lr-tb rl-tb tb-rl tb-lr lr-alternating-rl-tb lr-inverting-rl-tb lr-bt rl-bt bt-rl bt-lr lr-alternating-rl-bt lr-inverting-rl-bt tb-lr-in-lr-pairs lr rl tb inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template priority="5" match="fo:simple-page-master/@writing-mode">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> lr-tb rl-tb tb-rl tb-lr lr-alternating-rl-tb lr-inverting-rl-tb lr-bt rl-bt bt-rl bt-lr lr-alternating-rl-bt lr-inverting-rl-bt tb-lr-in-lr-pairs lr rl tb inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>


   <!-- 7.27. Miscellaneous properties -->
<xsl:template match="fo:external-graphic/@content-type                    | fo:instream-foreign-object/@content-type                    | rx:background-content-type"
                 priority="3">
      <xsl:variable name="val" select="normalize-space()"/>
      <xsl:choose>
         <xsl:when test="$val = 'auto'"/>
         <xsl:when test="starts-with($val, 'content-type:')"/>
         <xsl:when test="starts-with($val, 'namespace:')"/>
         <xsl:otherwise>
            <xsl:call-template name="print-warning">
               <xsl:with-param name="msg">Attribute '<xsl:value-of select="name()"/>' cannot have a value of "<xsl:value-of select="."/>": if not 'auto', it should be prefixed by either 'namespace:' or 'content-type:'.</xsl:with-param>
            </xsl:call-template>
         </xsl:otherwise>
      </xsl:choose>
   </xsl:template>


   <xsl:template match="@id"/>
   <xsl:template match="fo:page-number-citation/@ref-id"/>
   <xsl:template match="rx:page-number-citation-last/@ref-id"/>
   <xsl:template match="rx:end-index-range/@ref-id"/>

   <xsl:template match="@provisional-label-separation                    | @provisional-distance-between-starts">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@score-spaces">
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> true false inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="fo:external-graphic/@src">
      <xsl:call-template name="check-url"/>
   </xsl:template>


   <xsl:template priority="5" match="fo:color-profile/@src">
      <xsl:call-template name="check-url"/>
   </xsl:template>

   <xsl:template match="@visibility">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> visible hidden collapse inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@z-index">
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> auto inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>


   <!-- 7.28. Shorthands -->
<xsl:template priority="3" match="@background"/>
   <xsl:template priority="3" match="@background-position"/>
   <xsl:template priority="3" match="@border"/>
   <xsl:template priority="3"
                 match="@border-top                    | @border-bottom                    | @border-left                    | @border-right"/>
   <xsl:template priority="3" match="@border-color"/>
   <xsl:template priority="3" match="@border-style"/>
   <xsl:template match="@border-spacing"/>
   <xsl:template priority="3" match="@border-width"/>
   <xsl:template match="@cue"/>
   <xsl:template match="@font">
      <xsl:call-template name="disable-on-graphics"/>
   </xsl:template>
   <xsl:template match="@margin"/>
   <xsl:template priority="5"
                 match="fo:simple-page-master/@margin                    | fo:region-body/@margin"/>

   <xsl:template priority="3" match="@padding"/>

   <xsl:template match="@page-break-after | @page-break-before">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> auto always avoid left right inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@page-break-inside">
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> auto avoid inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@pause"/>

   <xsl:template match="@position">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> static relative absolute fixed inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template priority="5" match="fo:simple-page-master/@size">
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> auto landscape portrait </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@vertical-align">
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> baseline middle sub super text-top text-bottom top bottom inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@white-space">
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> normal pre nowrap inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@xml:lang">
      <xsl:call-template name="disable-on-graphics"/>
   </xsl:template>


   <!-- RenderX extensions -->

<xsl:template match="@rx:background-content-width                    | @rx:background-content-height"
                 priority="3">
      <xsl:call-template name="quantitative-values">
         <xsl:with-param name="valuelist"> auto scale-to-fit inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@rx:background-scaling" priority="3">
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> uniform non-uniform inherit </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="rx:meta-field/@name" priority="5"/>
   <xsl:template match="rx:meta-field/@value" priority="5"/>

   <xsl:template match="@rx:key | rx:begin-index-range/@key"/>

   <xsl:template match="rx:pinpoint/@value"/>

   <xsl:template match="rx:page-index/@ref-key | rx:index-item/@ref-key"/>

   <xsl:template match="@rx:list-separator | rx:page-index/@list-separator">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
   </xsl:template>

   <xsl:template match="@rx:range-separator | rx:page-index/@range-separator | rx:index-item/@range-separator">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
   </xsl:template>

   <xsl:template match="@rx:merge-subsequent-page-numbers | rx:page-index/@merge-subsequent-page-numbers | rx:index-item/@merge-subsequent-page-numbers">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> false true </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="@rx:link-back | rx:page-index/@link-back | rx:index-item/@link-back">
      <xsl:call-template name="disable-on-atomic-inlines"/>
      <xsl:call-template name="disable-on-graphics"/>
      <xsl:call-template name="enumerated-values">
         <xsl:with-param name="valuelist"> false true </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <!-- =================================== -->
<!-- Print an error message              -->

<xsl:template name="print-error">
      <xsl:param name="msg"/>
      <xsl:message>[error] <xsl:value-of select="normalize-space($msg)"/>
      </xsl:message>
   </xsl:template>

   <!-- =================================== -->
<!-- Print a warning message             -->

<xsl:template name="print-warning">
      <xsl:param name="msg"/>
      <xsl:message>[warning] <xsl:value-of select="normalize-space($msg)"/>
      </xsl:message>
   </xsl:template>

</xsl:stylesheet>