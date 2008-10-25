<script language="php">

require_once( 'roma/roma.php' );


$_SESSION['docLang'] = 'en';

switch( $_REQUEST[ 'option' ] )
  {
    case 'upload':
      $oRoma = new roma( roma_startupOption_upload );
      break;
    case 'minimum':
      $oRoma = new roma( roma_startupOption_minimum );
      break;
    case 'all':
      $oRoma = new roma( roma_startupOption_all );
      break;
    case 'other':
      $oRoma = new roma( roma_startupOption_other );
      break;
    default:
      $oRoma = new roma( '' );
      break;
  }

$Version = file_get_contents( roma_xquery_server . 'getversion.xq');
$_SESSION[ 'Version' ]  = $Version;
$oRoma->run();
</script>