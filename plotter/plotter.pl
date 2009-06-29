#!/usr/bin/perl

use strict;
use warnings;
use Cairo;
use Glib qw(TRUE FALSE);
use Gtk2 -init;

my $window = Gtk2::Window->new;
my $area = Gtk2::DrawingArea->new;
my $cr;

my $r_e = 6.357E6;
my $scale = 1/$r_e;


sub rotation {
	my $scale = shift @_;
	my $t = time;
	return ( sin($t) * $scale, cos($t) * $scale );
}

my $draw_sat = sub {
	my ($x, $y) = @_;
print STDERR 	"$x, $y\n";
	$cr->arc($x, $y, $r_e * 2, 0, 359);
	$cr->set_source_rgb(1,0,0);
	$cr->fill;
};

my $draw = sub {	
	return if ! $cr;

	$cr->set_source_rgb(0,0,0);
	$cr->paint;
	
	$cr->translate(300, 300);
	$cr->scale($scale, $scale);
	
	$cr->arc(0, 0, $r_e * 5, 0, 359);
	$cr->set_source_rgb(0,0,1);
	$cr->fill;
	
	$draw_sat->(rotation(100 * $r_e));
	
add_stuff_to_display_here:
	
	return TRUE;
};

$area->signal_connect( expose_event => sub {
	my ($widget, $event) = @_;

	$cr = Gtk2::Gdk::Cairo::Context->create($widget->window());
	$draw->();
			
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
