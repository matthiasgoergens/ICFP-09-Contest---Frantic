#!/usr/bin/perl -w

use FileHandle;

my $CFG = $ARGV[0];
$CFG or die "usage: $0 config [maxstep]";
my $maxstep = $ARGV[1];
$maxstep = 0xFFFFF if(!$maxstep); 

use IPC::Open2;
open2(\*READ, \*WRITE, "../HaskellVM/vm ../task/bin1.obf");
#open2(\*READ, \*WRITE, "./test.pl");

READ->autoflush(1);
WRITE->autoflush(1);

print WRITE "16000 $CFG\n";

for( my $i=0; $i < $maxstep ; $i++){
# print "$i\n";
    print WRITE ".\n";
        
    while( ($line = <READ>) ne ".\n" ){
      chomp $line;
#      print "<<<$line>>>\n";
      if($line =~ /^#time: (\d+)/){
        $time = $1;
      }elsif($line =~ /^#imp: /){
      
      }elsif($line =~ /^#out: /){
      
      }elsif( $line =~ /^(\d+) (-?[\d\.]+)/ ){
#        print STDERR "register found";
        $register{$1} = $2;
      }
    }
    
    print STDERR "time: $time\n";
    print STDERR "reg0: $register{0}\n";
    print STDERR "reg1: $register{1}\n";
    print STDERR "reg2: $register{2}\n";
    print STDERR "reg3: $register{3}\n";
} 
