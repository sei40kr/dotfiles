# launchctl.pl --- launchctl facade
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

my @launchctl_load_intermediate = ();

sub launchctl_load {
    my $service = $_[0];

    push( @launchctl_load_intermediate, $service );
}

my sub launchctl_load_reducer {
    return if ( scalar(@launchctl_load_intermediate) eq 0 );

    error('launchctl not found.') unless &is_macos;

    log_wait('Enabling launchctl services ...');

    Command::run( qw(launchctl -w load), @launchctl_load_intermediate );
}

register_reducer( 40, \&launchctl_load_reducer );

1;
