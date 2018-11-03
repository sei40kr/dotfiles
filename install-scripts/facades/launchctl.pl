# launchctl.pl --- launchctl facade
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use File::Basename qw(dirname);
use Cwd qw(realpath);

my $basepath                    = realpath( dirname(__FILE__) . "/../../" );
my @launchctl_load_intermediate = ();

sub launchctl_load {
    my $src = $_[0];

    $src = "${basepath}/${src}" if ( ( substr $src, 0, 1 ) ne '/' );

    push( @launchctl_load_intermediate, $src );
}

my sub launchctl_load_reducer {
    return if ( scalar(@launchctl_load_intermediate) eq 0 );

    error('launchctl not found.') unless &is_macos;

    log_wait('Enabling launchctl services ...');

    Command::run( qw(launchctl load -w), @launchctl_load_intermediate );
}

register_reducer( 40, \&launchctl_load_reducer );

1;
