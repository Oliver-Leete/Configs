#!/usr/bin/perl

use strict;

my %M;    # hash to hold keys from 'xdotool getmouselocation --shell'
my $pipe; # file handle for the pipes we're going to open

# get the current cursor location into $M{X} and $M{Y}
open($pipe, "-|", qw(xdotool getmouselocation --shell)) ||
    die "couldn't open pipe from xdotool: $!\n";

while(<$pipe>) {
  chomp;
  my($key, $val) = split /=/;
  $M{$key} = $val;
};
close($pipe);

# compare mouse location to monitor co-ordinates
open($pipe, "-|", "xrandr") ||
    die "couldn't open pipe from xrandr: $!\n";

while(<$pipe>) {
  my ($display, $width, $height, $x_offset, $y_offset);

  next unless m/ connected /;
  my @F = split;
  $display = $F[0];

  # co-ordinates are on fourth field (F[3]) on the primary display
  # or on third field (F[2]) on non-primary displays. Perl arrays
  # start from zero, not one (same as in bash). 
  if ($F[2] eq "primary") {
    ($width, $height, $x_offset, $y_offset) = split /[x+]/, $F[3];
  } else {
    ($width, $height, $x_offset, $y_offset) = split /[x+]/, $F[2];
  };

  if ($M{X} >= $x_offset && $M{X} <= $width + $x_offset && 
      $M{Y} >= $y_offset && $M{Y} <= $height + $y_offset) {
    print "$display\n";
    last;
  };
};
close($pipe);
