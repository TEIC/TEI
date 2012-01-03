print "<listObject>\n";
$n=0;
while (<>) {
chop;
s/&/&amp;/g;
 ($key,$rest) = /([A-Z]+) +(.*)/;
 if ($key eq 'NUMB') {
   if ($n > 0) { print "</object>\n"; }
   $n=$n+1;
   $rest =~ s/ /_/g;
   print qq(<object xml:id="$rest">\n);
 }
 elsif ($key eq 'CONT') { print qq(<idno type="CONT">$rest</idno>\n); }
 elsif ($key eq 'SMF') { print qq(<idno type="SMF">$rest</idno>\n); }
 elsif ($key eq 'SQU') { print qq(<idno type="SQU">$rest</idno>\n); }
 elsif ($key eq 'DESC') { print qq(<desc>$rest</desc>\n); }

}
print "</object></listObject>\n";
