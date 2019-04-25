# mas.pl --- mas facade
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/utils/installer/lib";
use Install::CommandRunner;

my @mas_install_intermediate = ();

sub mas_install {
    my $app_id = $_[0];

    push( @mas_install_intermediate, $app_id );
}

sub mas_reducer {
    return if ( scalar(@mas_install_intermediate) eq 0 );

    my @cmd = qw( mas install );
    if (&do_update) {
        push( @cmd, "--force" );
    }

    log_wait('Installing Mac App Store apps ...');

    run( @cmd, @mas_install_intermediate );
}

register_reducer( 41, \&mas_reducer );

1;
