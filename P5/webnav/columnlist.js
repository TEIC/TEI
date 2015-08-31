/** 
 * Copyright (c) 2007 Ingo Schommer (www.chillu.com) 
 * Licensed under the MIT License: 
 * http://www.opensource.org/licenses/mit-license.php 
 *  
 * Splits a <ul>/<ol>-list into equal-sized columns. 
 *  
 * Requirements:  
 * <ul> 
 * <li>All list-elements need to have the same height.</li> 
 * <li>List has to be blocklevel</li> 
 * </ul> 
 *  
 * Caution: margin-top/margin-left on <li> are overridden. 
 * Doesn't react to changes to the DOM, you need to call the function 
 * manually afterwards. 
 *  
 * @see http://www.alistapart.com/articles/multicolumnlists 
 */  
jQuery.fn.columnizeList = function(settings){  
    settings = jQuery.extend({  
        cols: 3,  
        width: '13',  
        unit: 'em'  
    }, settings);  
      
    var prevColNum = 0;  
    var size = $('li',this).size();  
    var computedColHeight = 0;  
    var baseFontSize = parseFloat($(this).css('font-size'));  
    $('li',this).each(function(i) {  
        var currentColNum = Math.floor(((i)/size) * settings.cols);  
        $(this).css('margin-left',(currentColNum*settings.width)+''+settings.unit);  
        if(prevColNum != currentColNum) {  
            $(this).css('margin-top','-'+(computedColHeight/baseFontSize)+'em');  
            computedColHeight = $(this).height();  
        } else {  
            $(this).css('margin-top','0');  
            computedColHeight += $(this).height();  
        }  
        prevColNum = currentColNum;  
    });  
  
    this.css('height',(size/settings.cols)*(parseFloat($('li:first',this).height())/baseFontSize)+'em');  
    this.after('<br style="clear: left;">');  
      
    var onchange = function(e) {  
        if(!e.originalTarget || e.originalTarget.tagName != 'LI') return true;  
        var scope = this; // caution: closure  
        setTimeout(function() {$(scope).columnizeList(settings);}, 50);  
    };  
      
    this.one('DOMNodeInserted',onchange);  
    this.one('DOMNodeRemoved',onchange);  
      
    return this;  
};  
  
