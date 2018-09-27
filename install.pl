#!/usr/bin/env perl
# -*- mode: cperl -*-

# install.pl --- dotfiles installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use File::Basename 'basename', 'dirname';
use Getopt::Long;
use Pod::Usage;

binmode STDOUT, ':encoding(UTF-8)';
binmode STDERR, ':encoding(UTF-8)';

$ENV{XDG_CONFIG_HOME} = "${ENV{HOME}}/.config"
  unless ( defined( $ENV{XDG_CONFIG_HOME} ) );
$ENV{XDG_DATA_HOME} = "${ENV{HOME}}/.local/share"
  unless ( defined( $ENV{XDG_DATA_HOME} ) );

my $basepath = dirname(__FILE__);

require $basepath . "/install-scripts/utility.pl";

GetOptions(
    'dry-run' => sub { set_dry_run(1); },
    update    => sub { set_update(1); },
    verbose   => sub { set_verbose(1); }
) or die 'Error in command-line arguments.';

my @modules;
if ( $#ARGV ne -1 ) { @modules = @ARGV; }
else {
    @modules =
      map { basename( $_, ".pl" ) }
      glob "${basepath}/install-scripts/modules/*.pl";
}

require $_ foreach glob "'${basepath}/install-scripts/facades/*.pl'";

require $basepath . "/install-scripts/modules/$_.pl" foreach @modules;

&run_reducers;
