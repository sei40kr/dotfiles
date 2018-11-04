# 10_trizen.pl ---
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use File::Path qw(rmtree);

my @trizen_sync_intermediate = ();

sub trizen_sync {
    my $pkg = $_[0];

    push( @trizen_sync_intermediate, $pkg );
}

my sub install_trizen {
    log_wait('Installing Trizen ...');

    git_clone_internal( 'https://aur.archlinux.org/trizen.git',
        'master', '/tmp/trizen' );

    print "> cd /tmp/trizen; makepkg -mis --noconfirm --needed\n"
      if ( &is_dry_run or &is_verbose );
    system( qw(sh -c), 'cd /tmp/trizen; makepkg -mis --noconfirm --needed' )
      unless (&is_dry_run);

    # TODO Create a symlink for Trizen config

    print "> rmtree('/tmp/trizen');\n" if ( &is_dry_run or &is_verbose );
    rmtree('/tmp/trizen') unless (&is_dry_run);
}

my sub trizen_sync_reducer {
    return if ( scalar(@trizen_sync_intermediate) eq 0 );

    error('trizen is available only on Arch Linux.') unless (&is_arch);

    # TODO Update Trizen itself when option --update given
    &install_trizen unless ( is_exec('trizen') );

    log_wait('Installing Trizen packages ...');

    my @cmd = qw(trizen -S --noedit --needed --noconfirm);
    push( @cmd, '--nopull' ) unless (&do_update);
    Command::run( @cmd, @trizen_sync_intermediate );
}

register_reducer( 40, \&trizen_sync_reducer );

1;
