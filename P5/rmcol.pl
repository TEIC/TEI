#!/usr/bin/perl -w
use SOAP::Lite;
die "Usage: $0 collection\n" unless @ARGV == 1;
my $collection = $ARGV[0];
my $service = SOAP::Lite->service("http://localhost:8080/cocoon/services/Admin?WSDL");
my $session = $service->connect("admin", "");
print "Removing collection $collection\n";
$service->removeCollection($session, $collection);
$service->disconnect($session);
