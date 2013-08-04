/* 
  This file provides popup functionality for footnotes in the Guidelines.
  Created by Martin Holmes, 2013-07, free for all for everything.
 */

$(document).ready(function(){
//First we create a popup box ready to receive the footnotes.
  var container = document.createElement('div');
  container.setAttribute('id', 'footnotePopup');
  var closer = document.createElement('div');
  closer.setAttribute('class', 'footnotePopupCloser');
  closer.setAttribute('onclick', 'this.parentNode.style.display = \'none\'');
  var x = document.createTextNode('x');
  closer.appendChild(x);
  var content = document.createElement('div');
  content.setAttribute('id', 'footnotePopupContent');
  container.appendChild(closer);
  container.appendChild(content);
  document.getElementsByTagName('body')[0].appendChild(container);

//Now we work through the links to footnotes.
  var links = document.getElementsByTagName('a');
  for (var i=0; i<links.length; i++){
    if (links[i].getAttribute('href').substring(0, 5) == '#Note'){
      if (links[i].getAttribute('class') != 'link_return'){
        links[i].setAttribute('onclick', 'showPopupFootnote(\'' + links[i].getAttribute('href').substring(1) + '\')');
        links[i].setAttribute('href', 'javascript:void(0)');
      }
    }
  }
});

function showPopupFootnote(footnoteId){
  var footnotePopup = document.getElementById('footnotePopup');
  var footnotePopupContent = document.getElementById('footnotePopupContent');
  var footnote = document.getElementById(footnoteId);
  if ((footnotePopup == null)||(footnotePopupContent == null)||(footnote == null)){
//If something is missing, we just default to original behaviour and jump to the footnote.
    document.location.hash = footnoteId;
    return;
  }
//Otherwise, we populate the popup with the content of the footnote, and show it.
  var cloneFootnote = footnote.cloneNode(true);
//We need to remove the id because it'll be a duplicate.
  cloneFootnote.setAttribute('id', '');
//Add it to the popup.
  clearContent(footnotePopupContent);
  footnotePopupContent.appendChild(cloneFootnote);
  footnotePopup.style.display = 'block';
}

function clearContent(targetEl){
  if (targetEl == null){return;}
  for (var i=targetEl.childNodes.length-1; i>-1; i--){
    targetEl.removeChild(targetEl.childNodes[i]);
  }
} 

//Bind the escape key so that it hides the popup if it's showing.
$(document).keyup(function(e){
  if(e.keyCode === 27)
    if (document.getElementById('footnotePopup').style.display == 'block'){
      document.getElementById('footnotePopup').style.display = 'none';
      e.preventDefault();
      e.stopPropagation();
    }
});


