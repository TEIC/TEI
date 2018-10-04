while (<>) {
if (/^#include /) {
my ($file) = /.include\s(.*)/;
open F,$file or die "cannot open $file\n";
while(<F>) { 
    s/</&lt;/g;
    s/>/&gt;/g;
    print; }
close F;
}
else
{print; }
}
