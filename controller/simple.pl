#!/usr/bin/perl -w
use strict;

my $CFG = $ARGV[0];
$CFG or die "usage: $0 config";

print "16000 $CFG\n";

my $i=0;

while (<STDIN>){   
    chomp;
    my ($time,$vx,$vy) = split / / , $_;
    if($time == -1){
	exit 0;
    }
    for( my $i=0; $i < $time ; $i++){
	print ".\n";
    } 
    print "2 " . $vx . "\n"; 
    print "3 " . $vy . "\n";
    print ".\n";
}
