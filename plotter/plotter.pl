#!/usr/bin/perl

use strict;
use warnings;
use Cairo;
use Glib qw(TRUE FALSE);
use Gtk2 -init;
use Time::HiRes qw(time);

my $window = Gtk2::Window->new;
my $area = Gtk2::DrawingArea->new;
my $cr;

my $r_e = 6.357E6;
my $scale = 1/$r_e * 100;
my $Pi = 3.14159265;

my $frames = 0;
my $starttime = time;

my ($width, $height);

$SIG{INT} = sub{ print STDERR "fps: ", $frames/(time - $starttime), "\n"; exit(0); };


sub rotation {
	my $scale = shift @_;
	my $t = time / $Pi * 10;
	
	return ( sin($t) * $scale, cos($t) * $scale );
}

my $clear_solid = sub {
	$cr->set_source_rgb(0,0,0);
	$cr->paint;
};

my $clear_blur = sub {
	$cr->set_source_rgba(0,0,0,0.01);
	$cr->rectangle(0, 0, $width, $height);
	$cr->fill;
};

my $clear = $clear_solid;

my $draw_sat = sub {
	my ($x, $y) = @_;
# print STDERR 	"$x, $y\n";
	$cr->arc($x, $y, $r_e * 0.01, 0, 359);
	$cr->set_source_rgb(1,0,0);
	$cr->fill;
};

my $parse_frame_task1 = sub{
    my %r;

    while(<>){
        if(/^\./){
            return %r;
        }elsif(/^#time: (\d+)/){
            $r{frame} = $1;
        }elsif(/0 (-?\d+\.?\d*e?-?\d+)/){
            $r{score} = $1;
        }elsif(/1 (-?\d+\.?\d*e?-?\d+)/){
            $r{fuel} = $1;
        }elsif(/2 (-?\d+\.?\d*e?-?\d+)/){
            $r{x} = $1;
        }elsif(/3 (-?\d+\.?\d*e?-?\d+)/){
            $r{y} = $1;
        }elsif(/4 (-?\d+\.?\d*e?-?\d+)/){
            $r{target_obit} = $1;
        }
    }

    return undef;
};

my $draw_frame_task1 = sub{
    my %frame = @_;

    $draw_sat->($frame{x}, $frame{y});
    $cr->set_source_rgb(0,1,0);

    my $lw;
    ($lw, $lw) = $cr->device_to_user_distance(1,1);

    $cr->set_line_width( $lw );
    $cr->arc(0,0, $r_e * 2, 0, 359);
    $cr->stroke;
};

my $parse_frame = $parse_frame_task1;
my $draw_frame = $draw_frame_task1;


my $draw = sub {	
	return if ! $cr;
	
	$frames++;

	$clear->();	
	
	$cr->translate(300, 300);
	$cr->scale($scale, $scale);
	
	$cr->arc(0, 0, $r_e, 0, 359);
	$cr->set_source_rgb(0,0,1);
	$cr->fill;
	
	#$draw_sat->(rotation(100 * $r_e));
  my %frame = $parse_frame->();
  $draw_frame->(%frame) if %frame;
	
	return TRUE;
};

$area->signal_connect( expose_event => sub {
	my ($widget, $event) = @_;

	$cr = Gtk2::Gdk::Cairo::Context->create($widget->window());
	$draw->();
	
	my ($x, $y);
	
	($x, $y, $width, $height) = $widget->window()->get_geometry;
			
	return FALSE;
} );

$window->signal_connect( delete_event => sub { Gtk2->main_quit; } );
$window->set_default_size(600, 600);
$window->add($area);
$window->show_all();

my $work = sub{
    $draw->();
    $area->queue_draw();
		
    return TRUE;
};

Glib::Idle->add($work);

Gtk2->main();
