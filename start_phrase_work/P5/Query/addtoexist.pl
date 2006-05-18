require LWP::UserAgent;
use Getopt::Long;
use Pod::Usage;

$COLLECTION = "/db";
$USER = "admin";
$PASS = "admin";
$HELP = 0;
$PUT = 0;
$QUERY = 0;

GetOptions(
    "u|user=s" => \$USER,
    "p|password=s" => \$PASS,
    "c|collection=s" => \$COLLECTION,
    "g|get=s" => \$GET,
    "s|store" => \$PUT,
    "r|remove=s" => \$REMOVE,
    "b|binary" => \$BINARY,
) or exit(1);

$URL = "http://$USER:$PASS\@localhost:8080/cocoon/servlet";
$ua = LWP::UserAgent->new();
if($REMOVE) {
    remove($REMOVE)
} elsif($PUT) {
    store();
} elsif($GET) {
    get($GET);
}

sub store {
    foreach $name (@ARGV) {    
        my $data = readFile($name);
        ($doc = $name) =~ s#.*/##s;
        print "Storing document as $doc to $URL$COLLECTION...\n";
     
        $req = HTTP::Request->new(PUT => "$URL$COLLECTION/$doc");
        if($BINARY) {
            $req->content_type('application/octet-stream');
        } else {
            $req->content_type('text/xml');
        }
        $req->content($data);

        $res = $ua->request($req);
        if($res->is_success) {
            print $res->content . "\n";
        } else {
            print "Error:\n\n" . $res->status_line . "\n";
        }
    }
}

sub remove {
    my($resource) = @_;

    print "Removing resource $resource ...\n";
    if($resource =~ /\/.*/) {
        $u = "$URL$resource";
    } else {
        $u = "$URL$COLLECTION/$resource";
    }
    print "$u\n";
    my $req = HTTP::Request->new(DELETE => $u);
    my $res = $ua->request($req);
    if($res->is_success) {
        print $res->content . "\n";
    } else {
        print "Error:\n\n" . $res->status_line . "\n";
    }
}


sub get {
    my($resource) = @_;
    my $u = $resource =~ /\/.*/ ? "$URL$resource" :
        "$URL$COLLECTION/$resource";
    my $req = HTTP::Request->new(GET => $u);
    my $res = $ua->request($req);
    if($res->is_success) {
        print $res->content . "\n";
    } else {
        print "Error:\n\n" . $res->status_line . "\n";
    }
}

sub readFile {
    my($file) = @_;
    open(XIN, $file);
    binmode XIN;
    my $pos = 0;
    while(($l = sysread(XIN, $xml, 4096, $pos)) > 0) {
        $pos = $pos + $l;
    }
    close(XIN);
    return $xml;
}

