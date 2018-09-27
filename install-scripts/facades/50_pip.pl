# 50_pip.pl --- pip facade
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

my @pip2_install_intermediate = ();
my @pip3_install_intermediate = ();

sub pip2_install {
    my $pkg = $_[0];

    push( @pip2_install_intermediate, $pkg );
}

sub pip3_install {
    my $pkg = $_[0];

    push( @pip3_install_intermediate, $pkg );
}

my sub pip2_install_reducer {
    return if ( scalar(@pip2_install_intermediate) eq 0 );

    is_exec('pip2') or error('pip2 not found.');

    log_wait('Installing Python2 packages ...');

    my @cmd = qw(pip2 install);
    push( @cmd, '-U' ) if (&do_update);
    run_cmd(@cmd, @pip2_install_intermediate);
}

my sub pip3_install_reducer {
    return if ( scalar(@pip3_install_intermediate) eq 0 );

    is_exec('pip3') or error('pip3 not found.');

    log_wait('Installing Python3 packages ...');

    my @cmd = qw(pip3 install);
    push( @cmd, '-U' ) if (&do_update);
    run_cmd(@cmd, @pip3_install_intermediate);
}

register_reducer( \&pip2_install_reducer );
register_reducer( \&pip3_install_reducer );
