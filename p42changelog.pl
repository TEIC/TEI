#!/usr/bin/perl
open (P, "p4 changes //TEI/P5/...|") or die "cant open pipe"; 
while (<P>) {
    ($CN)=/Change ([0-9]*) .*/;
    open (C,"p4 describe $CN |") or die "cannot open change $CN";
    while (<C>) {
	chop;
#Change 42984 by rahtz@spqr-dell-linux on 2003/11/23 12:35:40
	if (/^Change/) {
	    ($who,$where,$date,$time) = /Change [0-9]* by (.*)@(.*) on (.*) (.*)/;
	    if ($who eq 'syd')      { $Name="Syd Bauman"}
	    elsif ($who eq 'lou')   { $Name="Lou Burnard"}
	    elsif ($who eq 'rahtz') { $Name="Sebastian Rahtz"}
	    print "\n$date $Name <$who\@$where>\n\n";
	    print "\t*Change $CN\n";
	}
 	elsif (/^\t$/) {}
 	elsif (/^\t[A-z\<]/) {
	    s/\t//;
	    s/^<//;
	    s/>$//;
	    s/enter description here//;
	    print "\t$_\n";
	}
	elsif (/^\.\.\.\/\/TEI\/web/) { }
	elsif (/^\.\.\./) {
	    s+//TEI/P5/++;
	    print "\t$_\n";
	}
    }
    close C;
}
close P;

