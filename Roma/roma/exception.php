<script language="php">


class falseTagnameException extends Exception
  {
    private $m_szOldValue;
    private $m_szName;

    function __construct( $szName, $szOldValue ) 
      {
	parent::__construct();

	$this->m_szOldValue = $szOldValue;
	$this->m_szName = $szName;
      }

    function addError( $szName, $szLocation )
    {
	$oError = new error();
	$oError->setName( $szName );
	$oError->setLocation( $szLocation );
	$oError->setValue(  htmlentities( $this->m_szName ) );
	$oError->setOldValue( htmlentities( $this->m_szOldValue ) );
	$oError->addError();
      }
  }


class falseContentsException extends Exception
  {
    private $m_szOldValue;

    function __construct( $szOldValue ) 
      {
	parent::__construct();

	$this->m_szOldValue = $szOldValue;
      }

    function addError( $szName, $szLocation )
      { 
	$oError = new error();
	$oError->setName( $szName );
	$oError->setLocation( $szLocation );
	$oError->setOldValue( $this->m_szOldValue );
	$oError->addError();
      }
  }

class elementExistsException extends Exception
  {
    private $m_szOldValue;

    function __construct( $szOldValue ) 
      {
	parent::__construct();

	$this->m_szOldValue = $szOldValue;
      }

    function addError( $szName, $szLocation )
      { 
	$oError = new error();
	$oError->setName( $szName );
	$oError->setLocation( $szLocation );
	$oError->setOldValue( $this->m_szOldValue );
	$oError->addError();
      }
  }


class attributeExistsException extends Exception
  {
    private $m_szOldValue;

    function __construct( $szOldValue ) 
      {
	parent::__construct();

	$this->m_szOldValue = $szOldValue;
      }

    function addError( $szName, $szLocation )
      { 
	$oError = new error();
	$oError->setName( $szName );
	$oError->setLocation( $szLocation );
	$oError->setOldValue( $this->m_szOldValue );
	$oError->addError();
      }
  }


</script>