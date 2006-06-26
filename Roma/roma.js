var romaDivPrefix = 'descDiv_';
var romaSpanPrefix = 'descSpan_';

function descriptionPopup_Show( szId )
  {
    xPos = 0;
    yPos = 0;

    oSpan = document.getElementById( romaSpanPrefix + szId );

    if ( oSpan )
      {
        yPos = oSpan.offsetHeight;

        oTemp = oSpan;
        while( oTemp != null )
          {
            xPos += oTemp.offsetLeft;
            yPos += oTemp.offsetTop;
            oTemp = oTemp.offsetParent;
          }
      }

    oDiv = document.getElementById( romaDivPrefix + szId );

    if ( oDiv )
      {
        oDiv.style.left = xPos;
        oDiv.style.top = yPos;
        oDiv.style.visibility = 'visible';
      }

    oOtherDiv = document.getElementById( "HideItem" );

    if ( oOtherDiv )
      {
        oOtherDiv.style.visibility = 'hidden';
      }
  }

function descriptionPopup_Hide( szId )
  {
    xPos = 0;
    yPos = 0;
 
    oDiv = document.getElementById( romaDivPrefix + szId );

    if ( oDiv )
      {
        oDiv.style.left = xPos;
        oDiv.style.top = yPos;
        oDiv.style.visibility = 'hidden';
      }

    oOtherDiv = document.getElementById( "HideItem" );

    if ( oOtherDiv )
      {
        oOtherDiv.style.visibility = 'visible';
      }
  }



function addedElementsPopup_Show( )
  {
    xPos = 0;
    yPos = 0;

    oSpan = document.getElementById( "addedElementsSpan" );

    if ( oSpan )
      {
        yPos = oSpan.offsetHeight;

        oTemp = oSpan;
        while( oTemp != null )
          {
            xPos += oTemp.offsetLeft;
            yPos += oTemp.offsetTop;
            oTemp = oTemp.offsetParent;
          }
      }

    oDiv = document.getElementById( "addedElementsDiv" );

    if ( oDiv )
      {
        oDiv.style.left = xPos;
        oDiv.style.top = yPos;
        oDiv.style.visibility = 'visible';
      }

    oOtherDiv = document.getElementById( "HideItem" );

    if ( oOtherDiv )
      {
        oOtherDiv.style.visibility = 'hidden';
      }
  }

function addedElementsPopup_Hide( )
  {
    xPos = 0;
    yPos = 0;
 
    oDiv = document.getElementById( "addedElementsDiv" );

    if ( oDiv )
      {
        oDiv.style.left = xPos;
        oDiv.style.top = yPos;
        oDiv.style.visibility = 'hidden';
      }

    oOtherDiv = document.getElementById( "HideItem" );

    if ( oOtherDiv )
      {
        oOtherDiv.style.visibility = 'visible';
      }
  }


function addedAttributesPopup_Show( )
  {
    xPos = 0;
    yPos = 0;

    oSpan = document.getElementById( "addedAttributesSpan" );

    if ( oSpan )
      {
        yPos = oSpan.offsetHeight;

        oTemp = oSpan;
        while( oTemp != null )
          {
            xPos += oTemp.offsetLeft;
            yPos += oTemp.offsetTop;
            oTemp = oTemp.offsetParent;
          }
      }

    oDiv = document.getElementById( "addedAttributesDiv" );

    if ( oDiv )
      {
        oDiv.style.left = xPos;
        oDiv.style.top = yPos;
        oDiv.style.visibility = 'visible';
      }

    oOtherDiv = document.getElementById( "HideItem" );

    if ( oOtherDiv )
      {
        oOtherDiv.style.visibility = 'hidden';
      }
  }

function addedAttributesPopup_Hide( )
  {
    xPos = 0;
    yPos = 0;
 
    oDiv = document.getElementById( "addedAttributesDiv" );

    if ( oDiv )
      {
        oDiv.style.left = xPos;
        oDiv.style.top = yPos;
        oDiv.style.visibility = 'hidden';
      }

    oOtherDiv = document.getElementById( "HideItem" );

    if ( oOtherDiv )
      {
        oOtherDiv.style.visibility = 'visible';
      }
  }



function excludeAllElements()
  {
    oInputs = document.getElementsByTagName("input");

    if( oInputs )
      {
        for ( var i=0; i < oInputs.length; i++ )
	  {
	    if ( oInputs[i].value == "exclude" )
	      {
	        oInputs[i].checked = "true";
	      }
          }
      }
  }


function includeAllElements()
  {
    oInputs = document.getElementsByTagName("input");

    if( oInputs )
      {
        for ( var i=0; i < oInputs.length; i++ )
	  {
	    if ( oInputs[i].value == "include" )
	      {
	        oInputs[i].checked = "true";
	      }
          }
      }
  }



function excludeAllAttributes()
  {
    oInputs = document.getElementsByTagName("input");

    if( oInputs )
      {
        for ( var i=0; i < oInputs.length; i++ )
	  {
	    if ( oInputs[i].value == "no" )
	      {
	        oInputs[i].checked = "true";
	      }
          }
      }
  }


function includeAllAttributes()
  {
    oInputs = document.getElementsByTagName("input");

    if( oInputs )
      {
        for ( var i=0; i < oInputs.length; i++ )
	  {
	    if ( oInputs[i].value == "yes" )
	      {
	        oInputs[i].checked = "true";
	      }
          }
      }
  }

function setChangedClass(obj) {
   obj.parentNode.style.backgroundColor = "red";
   var box = document.getElementById('changedClasses');
   box.value='true';
}

function setChangedDesc(obj) {
   obj.style.border = "solid red 1pt";
   var box = document.getElementById('changedDesc');
   box.value='true';
}
function setChangedContent(obj) {
   obj.style.border = "solid red 1pt";
   var box = document.getElementById('changedContent');
   box.value='true';
}

