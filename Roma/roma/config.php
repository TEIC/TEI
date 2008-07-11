<?php

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
define( 'roma_pdflatex', 'pdflatex' );

// Defines if we are in debugging mode
define('DEBUG', false);

// Increasing the time limit
ini_set("max_execution_time", 60);

//#########################

define ( 'roma_temporaryFilesDir',   '/tmp' );
define ( 'roma_xquery_server',       'http://tei.oucs.ox.ac.uk/Query/' );
//define ( 'roma_xquery_server',       'http://localhost:8080/exist/rest/db/TEI/' );
//define('roma_xquery_server', 'http://www.tei-c.org/Query/');
define ( 'roma_teiweb_server',       'http://www.tei-c.org/release/doc/tei-p5-doc/' );
define ( 'roma_tei',                 '/usr/share/' );
define ( 'roma_localStylesheetDir',  'roma/stylesheets' );
define ( 'roma_templateDir',         'roma/templates' );
define ( 'roma_resource_path',       'roma/res' );
define ( 'roma_version','3.4');
define ( 'roma_date','2008-07-07');
?>