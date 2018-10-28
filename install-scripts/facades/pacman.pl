# 00_pacman.pl --- pacman facade
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

my @pacman_sync_intermediate = ();

sub pacman_sync {
    my $pkg = $_[0];

    push( @pacman_sync_intermediate, $pkg );
}

my sub pacman_sync_reducer {
    return if ( scalar(@pacman_sync_intermediate) eq 0 );

    unless (&is_arch) {

        # TODO
    }
    unless ( is_exec('pacman') ) {
        error('pacman not found.');
    }

    log_wait('Installing Pacman packages ...');

    Command::run( qw(sudo pacman -S --needed --noconfirm --noprogressbar),
        @pacman_sync_intermediate );
}

register_reducer( 0, \&pacman_sync_reducer );

1;
