# 40_systemctl.pl --- systemctl facade
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

my @systemctl_enable_intermediate      = ();
my @systemctl_enable_user_intermediate = ();

sub systemctl_enable {
    my $service = $_[0];

    push( @systemctl_enable_intermediate, $service );
}

sub systemctl_enable_user {
    my $service = $_[0];

    push( @systemctl_enable_user_intermediate, $service );
}

my sub systemctl_check_existence {
    error('systemctl not found.') unless is_exec('systemctl');
}

my sub systemctl_enable_reducer {
    return if ( scalar(@systemctl_enable_intermediate) eq 0 );

    &systemctl_check_existence;

    log_wait('Enabling systemctl services ...');

    Command::run( qw(sudo systemctl enable --now),
        @systemctl_enable_intermediate );
}

my sub systemctl_enable_user_reducer {
    return if ( scalar(@systemctl_enable_user_intermediate) eq 0 );

    &systemctl_check_existence;

    log_wait('Enabling systemctl user services ...');

    Command::run(
        qw(systemctl enable --user --now),
        @systemctl_enable_user_intermediate
    );
}

register_reducer( 40, \&systemctl_enable_reducer );
register_reducer( 40, \&systemctl_enable_user_reducer );

1;
