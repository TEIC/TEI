undef $/;
$_= <>;
s/^(<!ELEMENT[^\(>]*)(\([^>]+\))/$1." ". fixup($2) /emsg;
print $_;

sub fixup {
    my $what = shift;
    $what =~ s/$/ /;
    $what =~ s/([ |\,\(])([A-z0-9\.]+)/$1(%n.$2;)/g;
    $what =~ s/^\(\((%n.[A-z0-9\.]+;)\)([ \)])/\($1$2/g;
    $what =~ s/ \((%n.[A-z0-9\.]+;)\)([ \)])/ $1$2/g;
    $what =~ s/ \((%n.[A-z0-9\.]+;)\)$/ $1/g;
    $what =~ s/ $//;
    return $what;
}
