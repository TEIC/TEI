	$(document).ready(function(){

		/* Insert gloss at end of line */
		$('.l').each(function(index) {
			gloss_text = $(this).find('.gloss').contents().eq(1).text();
				$(this).html($(this).html() + '<span class="gloss-real">' + gloss_text + "</span>");
			});

		/* Set glossed words to look like links */
		editorial_annotated = $('.annotated:has(div.editorial)');
		editorial_annotated.addClass("editorial_annotated");

		/* Transfer content of original text to container */
		$('#holder').html($('.original').html());

		/* Show popup when span with an editorial annotation is clicked */

		function make_clickable() {
		$('.editorial_annotated').click(function(event){ 
			event.preventDefault();
			$('#popup').html($(this).children('.editorial').html());
			$('#popup').fadeIn(0);
			$('#popup').css({left: '100%', top: '100%'}); /* Establish where the corner of the page is*/
			if ($('#popup').offset().top > ($(this).offset().top + $('#popup').height()))
			{
				$('#popup').css({left: $(this).offset().left, top: $(this).offset().top + $(this).height()});
			}
			else 
			{
				if ($(0 < ($(this).offset().top - $('#popup').height()))) 
				{
					$('#popup').css({left: $(this).offset().left, top: $(this).offset().top - $('#popup').height()});
				}
			
				else 
				{
					$('#popup').css({left: $(this).offset().left, top: 0});
				}
			};
			

		});
		};
		make_clickable();
		
		$('#no-gloss').click(function() {
			if ($(this).is(':checked')) 
			{
				$('.gloss-real').fadeOut();
				$('#no-gloss-label').html('Show gloss');
			}
			else 
			{
				$('.gloss-real').fadeIn();
				$('#no-gloss-label').html('Hide gloss');
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



