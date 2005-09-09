<?xml version="1.0" encoding="UTF-8"?>
<XSLTdocConfig>
  <Title>The title used on the main page</Title>
  <Introduction>
    This section is copied to the main documentation page. It can include any HTML tags.
  </Introduction>
  
  <!-- 
       The absolute or relative path to the target directory 
       where the HTML files are created. If a relative path
       is used it is always relative to the config file.
  -->
  <TargetDirectory path="/TEI/Sourceforge/Stylesheets/doc/"/>
  <!-- 
       The absolute or relative path to the source directory.
       This is where the XSLT source files can be found.
       If a relative path is used it is always relative 
       to the config file.
  -->
  <SourceDirectory path="/TEI/Sourceforge/Stylesheets/html/"/>
  <!-- 
       A list of source code files which shall be documented. 
       Only stylesheets which are not included by another
       have to be listed here. The included stylesheets
       are found automatically by following the include or
       the import statements in the including stylesheet.
       Relative references are relative to the SourceDirectory
       defined above.
  -->
  <RootStylesheets>
    <File href="tei.xsl"/>
  </RootStylesheets>
  
  <!-- 
       A list of CSS Stylesheet files which should be added to the standard CSS file included (XSLTdoc.css)
       - Not mandatory
       - media attribute optional.
  -->
  <AdditionalCSS>
    <File href="print.css" media="print"/>
  </AdditionalCSS>
</XSLTdocConfig>
