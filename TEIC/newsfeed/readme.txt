
The TEI newsfeed consists of the creation of an Atom feed at Sourceforge with the wordpress system, 
and two HTML files 'news-body.html' and 'news-headlines.html'

The files in this directory:
atom2HTML-headlines.xsl Creates 'news-headlines.html' from the atom file.
atom2TEInews.xsl  Creates 'news-body.html' from the atom file.
htmlparse.xsl  David Carlisle's htmlparse for parsing CDATA escaped sections
old-atom-test-file.xml an old copy of the atom file.
readme.txt This file.
wget-news.sh  The shell script that grabs the atom file and ensures it is wellformed.

-James
