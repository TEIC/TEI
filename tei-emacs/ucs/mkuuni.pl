#
# Create uuni-[abcd].el data files
#

$dir = "Mule-UCS-0.72";

sub CreateFile {
    my ($letter, $id, @data) = @_;
    my $k;
    my $count = 0;
    my $idstring;
    if ($id > 0x100) {
	$idstring = pack "c", $id / 0x100;
    }
    $idstring .= pack "c", $id & 0xff;

    print STDERR "Writing uuni-$letter.el ... ";
    open (OUT, ">$dir/lisp/reldata/uuni-$letter.el")
	|| die "Cannot write to $uuni-$letter.el:$!\n";

    print OUT "; -*- coding: emacs-mule  -*-\n";
    print OUT ";;; uuni-$letter.el\n";
    print OUT ";;; Automatically generated, do not edit!\n";
    print OUT "(put 'unicode-$letter 'unicode-assoc\n";
    print OUT "     'unicode-$letter-vs-unicode-assoc)\n";
    print OUT "(defvar\n";
    print OUT "  unicode-$letter-vs-unicode-assoc\n";
    print OUT "  '(assoc\n";
    print OUT "   (char-1 . ucs-generic)\n";
    print OUT "   (\n";
    # print data
    foreach $k (@data) {
	print OUT "     (?$idstring";
	print OUT pack("c2", ($count / 0x60 + 0xa0), ($count % 0x60 + 0xa0));
	printf OUT " . ?\\x%04x)\n", $k;
	$count++;
    }
    print OUT ")))\n";
    print OUT "(provide 'uuni-$letter)\n";
    close OUT;
    print STDERR "done\n";
}

sub IsWide {
    my ($ucs) = @_;
    
    return
	(($ucs >= 0x2e80 && $ucs <= 0xa4cf && ($ucs & ~0x0011) != 0x300a &&
	  $ucs != 0x303f) ||                   # CJK ... Yi
	 ($ucs >= 0xac00 && $ucs <= 0xd7a3) || # Hangul Syllables
	 ($ucs >= 0xf900 && $ucs <= 0xfaff) || # CJK Compatibility Ideogr.
	 ($ucs >= 0xfe30 && $ucs <= 0xfe6f) || # CJK Compatibility Forms
	 ($ucs >= 0xff00 && $ucs <= 0xff5f) || # Fullwidth Forms
	 ($ucs >= 0xe000 && $ucs <= 0xefff) || # User-defined
	 ($ucs >= 0xffe0 && $ucs <= 0xffe6));
}


sub ReadCovered {
    %covered = ();

    my @datafiles = ( "ugb2312", "u-cns-1", "u-cns-2", "uksc5601", 
		      "ujisx0208", "ujisx0212" );
    my $file, $uni;

    foreach $file (@datafiles) {
	printf STDERR "Reading $file.el ... ";
	open (IN, "$dir/lisp/reldata/$file.el") 
	    || die "Cannot read $file:$!\n";
	while (<IN>) {
	    chop;
	    if (/^\s*\(.*\.\s+\?\\x([0-9a-fA-F]+)\)/) {
		$uni = hex($1);
		$covered{$uni} = 1;
	    }
	}
	close IN;
	printf STDERR "done\n";
    }
}

sub DumpDate {
    open (MONO, ">mono.txt") || die "Cannot write to mono.txt:$!\n";
    foreach $k (@mono) {
	printf MONO "%04x\n", $k;
    }
    close MONO;
}

&ReadCovered;

@unia = ();
@unib = ();
@unic = ();
@unid = ();
@unie = ();

print STDERR "Creating mapping table ... ";
for ($ucs = 0x0080; $ucs < 0x10000; $ucs++) {
    next if 0xd800 <= $ucs && $ucs < 0xe000;
    if (&IsWide($ucs)) {
	if (defined($covered{$ucs})) {
	    # nothing
	} else {
	    if ($#unic < 0x23ff) {
		push(@unic, $ucs);
	    } elsif ($#unid < 0x23ff) {
		push(@unid, $ucs);
	    } else {
		push(@unie, $ucs);
	    }
	}
    } else {
	if ($#unia < 0x23ff) {
	    push(@unia, $ucs);
	} else {
	    push(@unib, $ucs);
	}
    }
}
print STDERR "done\n";

&CreateFile("a", 0x9cf2, @unia);
&CreateFile("b", 0x9cf3, @unib);
&CreateFile("c", 0x97, @unic);
&CreateFile("d", 0x9dfd, @unid);
&CreateFile("e", 0x9dfe, @unie);
