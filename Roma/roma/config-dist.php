<script language="php">

session_start(); 

//#########################
// Program calls
//
// Defines how to call trang
define( 'roma_trang', '/usr/bin/trang' );

// Defines how to call fop
define( 'roma_fop', 'fop' );

// Defines how to call latex
define( 'roma_latex', 'latex' );

// Defines how to call pdflatex
define( 'roma_pdflatex', 'xelatex' );

// Defines if we are in debugging mode
define('DEBUG', false);

// Increasing the time limit
ini_set("max_execution_time", 60);


//#########################
// for www.tei-c.org
define ( 'oxgarage_server',          'http://www.tei-c.org/ege-webservice');
define ( 'roma_xquery_server',       'http://www.tei-c.org/Query/' );
define ( 'roma_teiweb_server',       'http://www.tei-c.org/release/doc/tei-p5-doc/' );
// for tei.oucs.ox.ac.uk
//define ( 'oxgarage_server','http://oxgarage.oucs.ox.ac.uk:8080/ege-webservice');
//define ( 'roma_xquery_server', 'http://tei.oucs.ox.ac.uk/Query/' );

define ( 'roma_customization_validator','roma/oddschema.rng');
define ( 'roma_temporaryFilesDir',   '/tmp' );
define ( 'roma_tei',                 '/usr/share/' );
define ( 'roma_localStylesheetDir',  'roma/stylesheets' );
define ( 'roma_templateDir',         'roma/templates' );
define ( 'roma_resource_path',       'roma/res' );
define ('roma_version','4.7');
define ('roma_date','2011-11-17');
</script>
