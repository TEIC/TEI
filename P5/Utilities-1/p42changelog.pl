#!/usr/bin/perl
open (P, "p4 changes //TEI/P5/...|") or die "cant open pipe"; 
open CH, "> ChangeLog-preCVS";
print <<EOF;
	<html>
	<head>
	<title>TEI Perforce version control system change</title>
	</head>
	<body>
	<h1>TEI Perforce version control system</h1>
        <ul>
EOF
while (<P>) {
    ($CN)=/Change ([0-9]*) .*/;
    print "<li><a href=\"teichange-$CN.html\">$_</a></li>\n";
    open (C,"p4 describe $CN |") or die "cannot open change $CN";
    open O, "> teichange-$CN.html";
    print O <<EOF;
	<html>
	<head>
	<title>TEI Perforce version control system, Change $CN</title>
	</head>
	<body>
	<h1>TEI Perforce version control system, Change $CN</h1>
	<pre>
EOF
    while (<C>) {
	s/&/&amp;;/g;
	s/</&lt;/g;
	s/>/&gt;/g;
	print O;
    }
    close C;
    print O <<EOF;
	</pre>
	</body>
	</html>
EOF
    close O;
    open (C,"p4 describe $CN |") or die "cannot open change $CN";
    while (<C>) {
	chop;
#Change 42984 by rahtz@spqr-dell-linux on 2003/11/23 12:35:40
	if (/^Change/) {
	    ($who,$where,$date,$time) = /Change [0-9]* by (.*)@(.*) on (.*) (.*)/;
	    if ($who eq 'syd')      { $Name="Syd Bauman"}
	    elsif ($who eq 'lou')   { $Name="Lou Burnard"}
	    elsif ($who eq 'rahtz') { $Name="Sebastian Rahtz"}
	    print CH  "\n$date $Name <$who\@$where>\n\n";
	    print CH  "\t*Change $CN\n";
	}
 	elsif (/^\t$/) {}
 	elsif (/^\t[A-z\<]/) {
	    s/\t//;
	    s/^<//;
	    s/>$//;
	    s/enter description here//;
	    print CH  "\t$_\n";
	}
	elsif (/^\.\.\.\/\/TEI\/web/) { }
	elsif (/^\.\.\./) {
	    s+//TEI/P5/++;
	    print CH  "\t$_\n";
	}
    }
    close C;
}
print <<EOF;
</ul>
</body>
</html>
EOF
close P;
close CH;
