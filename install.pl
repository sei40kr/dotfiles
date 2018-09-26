#!/usr/bin/env perl
# -*- mode: cperl -*-
use utf8;
use strict;
use warnings;
use File::Basename 'basename', 'dirname';
use Getopt::Long;
use Pod::Usage;

# install.pl --- dotfiles installer
# author: Seong Yong-ju <sei40kr@gmail.com>

binmode STDOUT, ':utf8';

my $basepath = dirname(__FILE__);

my %options = ();

GetOptions( \%options, 'dry-run', 'update', 'verbose' )
  or die 'Error in command-line arguments.';

my @modules;
if ( $#ARGV ne -1 ) { @modules = @ARGV; }
else {
    @modules =
      map { basename( $_, ".pl" ) }
      glob "${basepath}/install-scripts/modules/*.pl";
}

foreach my $path ( glob "'${basepath}/install-scripts/facades/*.pl'" ) {
    require "${path}";
}

foreach my $module (@modules) {
    require "${basepath}/install-scripts/modules/${module}.pl";
}
