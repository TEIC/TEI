<script language="php">
//#########################
// Programm calls
//
// Defines how to call trang
define( 'roma_trang', 'trang' );

// Defines how to call fop
define( 'roma_fop', 'fop' );

// Defines how to call latex
define( 'roma_latex', 'latex' );

// Defines how to call pdflatex
define( 'roma_pdflatex', 'pdflatex' );


//#########################

define( 'roma_temporaryFilesDir', '/tmp' );
define( 'roma_schemaDir', 'http://localhost/schema' );
define( 'roma_StylesheetDir', 'http://localhost/stylesheet' );

define( 'roma_localStylesheetDir', 'roma/stylesheets' );
define( 'romadom_templateDir', 'roma/templates' );
define( 'roma_templateDir', 'roma/templates' );
define( 'roma_ressource_path', 'roma/res' );

//#########################
// different_stylesheets
//
define( 'roma_styleheet_docHtml', $roma_StylesheetDir . '/teic/teihtml-teic-P5.xsl' );
define( 'roma_styleheet_docPDF', $roma_StylesheetDir . '/base/p5/fo/tei.xsl' );
define( 'roma_styleheet_docLatex', $roma_StylesheetDir . '/base/p5/latex/teilatex.xsl' );
define( 'roma_customization_validator', $roma_schemaDir . 'schema/relaxng/p5/p5odds.rng' );
define( 'roma_xquery_server', 'http://localhost:8080/cocoon/Roma/xquery' );
</script>