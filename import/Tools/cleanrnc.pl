while (<>) {
 if (/^namespace may /) { print; }
 elsif (/namespace</) { print; }
 elsif (/^namespace by /) { print; }
 elsif (/^namespace apply /) { print; }
 elsif (/^namespace remains /) { print; }
 elsif (/^namespace in /) { print; }
 elsif (/^namespace /) {  }
 elsif (/^default namespace /) { }
 elsif (/<term>namespace/) {print; }
 elsif (/<gloss>namespace/) {print; }
 elsif (/>namespace/){
	s/(.*>)namespace.*/$1/;
	print;
	while (<>) {
	    if (/</) { print; last; }
	    elsif (/^namespace /) {  }
	    elsif (/^default namespace /) { }
 	    elsif (/^$/) { }
	    else {print; }
	}
    }
    else { 
	s/>namespace local = \"\"/>/;
	print; }
}
