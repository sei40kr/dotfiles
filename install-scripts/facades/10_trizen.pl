# 10_trizen.pl ---
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

my @trizen_sync_intermediate = ();

sub trizen_sync {
    my $pkg = $_[0];

    push( @trizen_sync_intermediate, $pkg );
}

my sub trizen_sync_reducer {
    return if ( scalar(@trizen_sync_intermediate) eq 0 );

    is_exec('trizen') or error('trizen not found.');

    log_wait('Installing Trizen packages ...');

    my @cmd = qw(trizen -S --noedit --needed --noconfirm);
    push( @cmd, '--nopull' ) if (&do_update);
    run_cmd(@cmd, @trizen_sync_intermediate);
}

register_reducer( \&trizen_sync_reducer );

1;
