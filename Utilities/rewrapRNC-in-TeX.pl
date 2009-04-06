#!/usr/bin/perl
while (<>) {
      if ( / \| /) {
	  chop;
	  $line = $_;
	  $N=length($line);
	  if ($N > 120) {
	      $_=substr($line,1,90);
	      my ($indent,$first) = /([ ]*)(.*)/;
	      print $indent;
	      print $first;
	      $_=substr($line,91);
	      my ($bef,$after) = /([^\|]*)(\|?.*)/;
	      print "$bef\n$indent$after\n";
	  }
	  else
	  {
	      print "$_\n";
	  }

      }
else 
     {
	 print;
     }
}
