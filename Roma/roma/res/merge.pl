#!/usr/bin/perl
use Getopt::Long;
use utf8;
$LANG="";
$FILE="";
$thislang="whatever";
GetOptions(
    "l|lang=s" => \$LANG,
    "f|file=s" => \$FILE,
	   ) or exit(1);
if ($LANG eq '') {
    print "no language specified";
    exit(1);
}
if ($FILE eq '') {
    print "no file specified";
    exit(1);
}
my $f = $LANG . "/" . $FILE;
open N, "<:utf8", $f or die "cannot open " . $f;
while (<N>) {
    if (/\[/) {
	($thislang) = /.*\[(.*)\]/;
    }
    elsif (/^$/) {}
    else
    {
	$translations{$thislang} .= $_;
    }
}
close N;

open N,"<:utf8", $FILE or print "cannot open " . $FILE;
while (<N>) {
    if (/\[/) {
	($thislang) = /.*\[(.*)\]/;
    }
    elsif (/^$/) {}
    else
    {
	$Old{$thislang} .= $_;
    }
}

binmode( STDOUT, ':utf8' );
foreach  (sort keys %Old) {
    if ($_ ne $LANG) {
	print "[" . $_ . "]\n";
	print $Old{$_};
	print "\n\n";
    }
}
print "[" . $LANG . "]\n";
print $translations{$LANG};
print "\n\n";
close N;
