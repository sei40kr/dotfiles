# tic.pl --- tic facade
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

my @tic_intermediate = ();

sub tic {
    my $file = $_[0];

    push( @tic_intermediate, $file );
}

my sub tic_reducer {
    return if ( scalar(@tic_intermediate) eq 0 );

    error('tic not found.') unless is_exec('tic');

    log_wait('Installing terminfo files ...');

    Command::run( qw(tic -x), $_ ) foreach @tic_intermediate;
}

register_reducer( 70, \&tic_reducer );

1;
