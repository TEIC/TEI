#!/bin/bash
#Variables
OUTPUTDIR="/projects/tei/web/include/"
SCRIPTDIR="/home/oxford/newsfeed/"
#OUTPUTDIR="/home/jamesc/tmp/TEI-newsfeed/"
#SCRIPTDIR="/home/jamesc/tmp/TEI-newsfeed/"
INCOMING="incoming.xml"
EXISTING="atom.xml"
#FEED="https://sourceforge.net/apps/wordpress/tei/feed/atom/"
FEED="https://textencodinginitiative.wordpress.com/feed/atom/"

# move to output directory
cd $OUTPUTDIR

# update time on existing file or if non-existent create it
touch $EXISTING

# wget the feed
wget --no-check-certificate -q -O $INCOMING $FEED

# if identical then stop
if  cmp $INCOMING $EXISTING &>/dev/null
then  echo "INCOMING FILE IDENTICAL TO EXISTING FILE" 
rm -f $INCOMING
exit 0
fi

# if not well-formed then error
if ! xmllint --noout $INCOMING &>/dev/null
then  echo "ERROR: INCOMING FILE IS NOT WELL FORMED" 
rm -f $INCOMING
exit 1
fi

# if doesn't at least have a '<entry' in it then error
if ! grep -q "<entry" $INCOMING  &>/dev/null
then echo "ERROR: NO ENTRY ELEMENT IN INCOMING FILE"
rm -f $INCOMING
exit 1
fi

# Create updated.txt and check if it says YES
saxon -it main -o updated.txt $SCRIPTDIR/isUpdated.xsl incoming=$OUTPUTDIR$INCOMING existing=$OUTPUTDIR$EXISTING ;
if ! grep -q "YES" updated.txt &>/dev/null
then echo "ERROR: NOT UPDATED FEED DATE"
rm -f $INCOMING
rm -f updated.txt
exit 1
fi

#Commented out below test for file size to help with corrections.

#get bytesizes of files for later comparison
#INCOMINGFILESIZE=$(stat -c%s "$INCOMING")
#EXISTINGFILESIZE=$(stat -c%s "$EXISTING")

# if less than or equal filesize don't update
#if [ "$INCOMINGFILESIZE" -le "$EXISTINGFILESIZE"  ]
#then echo "INCOMING FILE IS SMALLER THAN EXISTING"
#rm -f $INCOMING
#exit 0
#fi

# create temporary news body file
saxon -o news-body-temp.html -s $INCOMING $SCRIPTDIR/atom2TEInews.xsl ;

# if temporary news body file is not well formed, error
if ! xmllint --noout news-body-temp.html &>/dev/null
then echo "ERROR: news-body-temp.html IS NOT WELL-FORMED"
rm -f $INCOMING
exit 1
fi

# create temporary news headlines file
saxon -o news-headlines-temp.html -s $INCOMING $SCRIPTDIR/atom2HTML-headlines.xsl ;

# if temporary news headlines file is not well formed, error
if ! xmllint --noout news-headlines-temp.html &>/dev/null
then echo "ERROR: news-headlines-temp.html IS NOT WELL-FORMED"
rm -f $INCOMING
exit 1
fi

# create backup of feed in news-old/
cp $EXISTING news-old/`date +%FT%T`.atom.xml;

#overwrite existing with incoming
mv -f $INCOMING $EXISTING;

# move temporary html files to real place
mv -f news-headlines-temp.html news-headlines.html
mv -f news-body-temp.html news-body.html

# double-check permissions on everything
chmod ugo+rwx atom.xml
chmod ugo+rwx news-old/*.xml
chmod ugo+rwx news*.html

mail -s "New Posting on TEI-C" james+TEINewsfeed@blushingbunny.net <atom.xml
#done
