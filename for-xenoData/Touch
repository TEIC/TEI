#!/bin/sh
cd `dirname $0`
cd ..
touch /tmp/changed
/usr/bin/svn -q update P5 Stylesheets I18N
find P5 Stylesheets -newer /tmp/changed | grep -v .svn > /tmp/changed.files
if test -s /tmp/changed.files 
then 
 cd P5
 make html-web
 make exemplars
 make LANGUAGE=es DOCUMENTATIONLANGUAGE=es html-web
 make LANGUAGE=de DOCUMENTATIONLANGUAGE=de html-web
 make LANGUAGE=ja DOCUMENTATIONLANGUAGE=ja html-web
 make LANGUAGE=fr DOCUMENTATIONLANGUAGE=fr html-web
 make LANGUAGE=it DOCUMENTATIONLANGUAGE=it html-web
 make LANGUAGE=kr DOCUMENTATIONLANGUAGE=kr html-web
 make LANGUAGE=zh-TW DOCUMENTATIONLANGUAGE=zh-TW html-web
 cp Guidelines.pdf Guidelines-web/en
fi
rm /tmp/changed.files /tmp/changed
