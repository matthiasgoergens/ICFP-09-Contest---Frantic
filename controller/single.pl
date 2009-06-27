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

$std_command = sub{
  print WRITE ".\n";
};

$command = $std_command;

for( my $i=0; $i < $maxstep ; $i++){
# print "$i\n";
    
    $command->();
            
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
    
    $fuel = $register{1};
    $sx = $register{2};
    $sy = $register{3};
    $target_orbit_radius = $register{4};
    
    if(defined $last_sx && defined $last_sy){
      $vx = $last_sx - $sx;
      $vy = $last_sy - $sy;
      $v = sqrt( ($sx - $last_sx)**2 + ($sy - $last_sy)**2 );
      
      $m_e = 6E24;
      $r_e = 6.357E6;
      $G = 6.67428E-11;
      
      $GM = 398600.4418;
      
      $r = sqrt( $sx**2 + $sy**2 );
      
      print STDERR "speed: $v, radius: $r";
    }
    
    if( $time == 2 ){
      my $nvx = -$sy * 1/$r * 2466.49;
      my $nvy = -$sx * 1/$r * 2466.49;      
      
      $command = sub{
        print WRITE "2 $nvx\n";
        print WRITE "3 $nvy\n";
        print WRITE "16000 $CFG\n";
        print WRITE ".\n";
        
        $command = $std_command;
      };
    }
    
    $last_sx = $sx;
    $last_sy = $sy;
} 
