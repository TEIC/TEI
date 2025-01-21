/* 
  This file provides popup functionality for footnotes and bibliography references
  in the Guidelines.
  
  Created by Martin Holmes, 2013-07 and 2014-07, free for all for everything.
 */
 
 //Make these pointers global so we can use them everywhere.
 var footnotePopup = null;
 var footnotePopupContent = null;
 var biblFrame = null;


document.addEventListener("DOMContentLoaded", function() {
  footnotePopup = document.createElement('div');
  footnotePopup.setAttribute('id', 'footnotePopup');
  var closer = document.createElement('div');
  closer.setAttribute('class', 'footnotePopupCloser');
  closer.onclick = function() {this.parentNode.style.display = "none"};
  var x = document.createTextNode('x');
  closer.appendChild(x);
  footnotePopupContent = document.createElement('div');
  footnotePopupContent.setAttribute('id', 'footnotePopupContent');
  footnotePopup.appendChild(closer);
  footnotePopup.appendChild(footnotePopupContent);
  document.getElementsByTagName('body')[0].appendChild(footnotePopup);

//Now we work through the links to footnotes.
  var links = document.getElementsByTagName('a');
  for (var i=0; i<links.length; i++){
    if (links[i].hasAttribute("href") && links[i].getAttribute('href').substring(0, 5) == '#Note'){
      if (links[i].getAttribute('class') != 'link_return'){
        var dest = links[i].getAttribute('href').substring(1);
        links[i].onclick = function (e) {
          e.preventDefault();
          showPopupFootnote(e.target, dest);
        }
      }
    }    
  }
  addBiblFrame();
});

function showPopupFootnote(el, footnoteId){
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
  // Also remove links back to the text because they are not needed in popups.
  cloneFootnote.querySelectorAll("a.link_return").forEach(function(a) {a.parentElement.removeChild(a)});
//Add it to the popup.
  clearContent(footnotePopupContent);
  footnotePopupContent.appendChild(cloneFootnote);
  footnotePopup.style.display = 'block';

  // Position the popup near the footnote
  const footnoteY = el.getBoundingClientRect().top;
  const popupHeight = footnotePopup.getBoundingClientRect().height;
  footnotePopup.style.top = (footnoteY - popupHeight - 20) + 'px';
}

function clearContent(targetEl){
  if (targetEl == null){return;}
  for (var i=targetEl.childNodes.length-1; i>-1; i--){
    targetEl.removeChild(targetEl.childNodes[i]);
  }
} 

//Bind the escape key so that it hides the popup if it's showing.
document.addEventListener("keyup", function(e) {
  if(e.keyCode === 27) {
    if (document.getElementById('footnotePopup').style.display == 'block'){
      document.getElementById('footnotePopup').style.display = 'none';
      e.preventDefault();
      e.stopPropagation();
    }
  }
})

//These functions set up and handle the display of bibliographical references
//as popups.

//This function finds all links to items in the bibliography and turns them
//into JS calls which retrieve the content which has been imported into 
//an iframe, and display it as a popup.
function setupBiblPopups () {
  var links = document.getElementsByTagName('a');
  for (var i=0; i<links.length; i++){
    var href = links[i].getAttribute('href');
    if (href && href.substring(0, 9) == 'BIB.html#'){
      var biblId = href.substring(9, href.length);
        links[i].onclick = function(e) {
          e.preventDefault();
          showPopupBibl(e.target, biblId)
        }
      //}
    }
  }
}

//This function creates an invisible iframe and loads the BIB.html document
//into it, so we can retrieve references from it and show them in popups.
function addBiblFrame(){
//First make sure we're not running this on the bibliography itself.
    var biblDoc = new RegExp('BIB\.html(#.+)?$');
    var isBibliography = biblDoc.exec(window.location);
    if (isBibliography){return;}
    biblFrame = document.createElement('iframe');
    biblFrame.style.display = 'none';
    document.getElementsByTagName('body')[0].appendChild(biblFrame);
    biblFrame.setAttribute('src', 'BIB.html');
    biblFrame.addEventListener("load", setupBiblPopups);
}

//This function shows a bibl popup. It differs slightly from the function for note
//popups, so it is distinct.
function showPopupBibl(el, biblId){
  var bibl = null;
//We have to be cautious here; some browsers block access to the iframe 
//document contents from another document, especially when running locally.
  try{
    bibl = biblFrame.contentDocument.getElementById(biblId);
  }
  catch(e){
    document.location = 'BIB.html#' + biblId;
    return;
  }
  
  if ((footnotePopup == null)||(footnotePopupContent == null)||(bibl == null)){
//If something is missing, we just default to original behaviour and jump to the bibliography.
    document.location = 'BIB.html#' + biblId;
    return;
  }
//Otherwise, we populate the popup with the content of the bibl, and show it.
  var biblContent = bibl.cloneNode(true);
  // Also remove links back to the text because they are not needed in popups.
  biblContent.querySelectorAll("a.link_return").forEach(function(a) {a.parentElement.removeChild(a)});
//Add it to the popup.
  clearContent(footnotePopupContent);
  footnotePopupContent.innerHTML = biblContent.innerHTML;
  footnotePopup.style.display = 'block';

  // Position the popup near the footnote
  const footnoteY = el.getBoundingClientRect().top;
  const popupHeight = footnotePopup.getBoundingClientRect().height;
  footnotePopup.style.top = (footnoteY - popupHeight - 20) + 'px';
}


