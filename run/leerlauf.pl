#!/usr/bin/perl -w

my $CFG = $ARGV[0];
$CFG or die "usage: $0 config [maxstep]";
my $maxstep = $ARGV[1];
$maxstep = 0xFFFFF if(!$maxstep); 

print "16000 $CFG\n";

for( my $i=0; $i < $maxstep ; $i++){
    print ".\n"
} 
