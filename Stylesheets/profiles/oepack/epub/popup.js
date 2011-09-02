	$(document).ready(function(){
		
		/* Make all annotated words look like links*/
		$('.note.editorial > a').each(function() {
			annotated = $(this).attr('href');
			$(annotated).addClass('annotated');
		});

		/* Make all glossed words have a 'degree' symbol*/
		$('.note.gloss > a').each(function() {
			annotated = $(this).attr('href');
			$(annotated).html($(annotated).html() + '<sup>o</sup>'); /*This should be a degree symbol*/
		});

		/* Show popup at the position of 'element', with the contents 'contents'*/
		function popup(element, contents) {
			event.preventDefault();
			$('#popup').html(contents);
			$('#popup').fadeIn(0);
			$('#popup').css({left: '100%', top: '100%'}); /* Establish where the corner of the page is*/
			if ($('#popup').offset().top > (element.offset().top + $('#popup').height()))
			{
				$('#popup').css({left: element.offset().left, top: element.offset().top + element.height()});
			}
			else 
			{
				if ($(0 < (element.offset().top - $('#popup').height()))) 
				{
					$('#popup').css({left: element.offset().left, top: element.offset().top - $('#popup').height()});
				}
			
				else 
				{
					$('#popup').css({left: element.offset().left, top: 0});
				}
			};
			

		};
	
		function make_clickable() {
		$('span.annotated').click(function(event)
			{
				id = $(this).attr('id');
				glossnote = ($('.gloss.note [href="#' + id + '"]'));
				editorialnote = ($('.editorial.note [href="#' + id + '"]'));
				popup($(this), editorialnote.text());
		
			});
	/* Doesn't work in touch interfaces, but still nice*/
		$('.gloss > a').mouseover(function(event) {
			id = $(this).attr('href');
			$(id).css('background', 'yellow');
		});			

		$('.gloss > a').mouseleave(function(event) {
			id = $(this).attr('href');
			$(id).css('background', '');
		});			

		}

		make_clickable();

		$('#no-gloss').click(function() {
			if ($(this).html() == 'Hide gloss') 
			{
				$('.gloss-real').fadeOut();
				$('#no-gloss').html('Show gloss');
			}
			else 
			{
				$('.gloss-real').fadeIn();
				$('#no-gloss').html('Hide gloss');
			}
			});

		$('#popup').click(function(){
				$('#popup').fadeOut();
				});

		$('#version-switch').click(function(event){
			
		function change_sides(front) {
			if (front) {
				$('#holder').html($('.original').html());
				make_clickable();
			}
			else {
				$('#holder').html($('.translation').html());
			}
			}

			event.preventDefault();
				$('#popup').fadeOut(0);
				$('#holder').rotate3Di('toggle', 1000, {sideChange: change_sides});
			});
	
	});



