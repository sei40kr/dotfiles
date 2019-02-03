# 50_pip.pl --- pip facade
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/utils/installer/lib";
use Install::CommandRunner;
use Install::Logger;

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

my sub find_pip2_exec {
    foreach
      my $dirpath ( "${ENV{HOME}}/.pyenv/shims", "/usr/local/bin", "/usr/bin" )
    {
        return "${dirpath}/pip2" if ( -x "${dirpath}/pip2" );
    }
}

my sub find_pip3_exec {
    foreach
      my $dirpath ( "${ENV{HOME}}/.pyenv/shims", "/usr/local/bin", "/usr/bin" )
    {
        return "${dirpath}/pip3" if ( -x "${dirpath}/pip3" );
    }
}

my sub pip2_install_reducer {
    push( @pip2_install_intermediate, 'pip' ) if (&do_update);
    return if ( scalar(@pip2_install_intermediate) eq 0 );

    log_wait('Installing Python2 packages ...');

    my $pip2_exec = &find_pip2_exec;
    error('pip2 is not found.') unless ( defined($pip2_exec) );

    my @args = qw(install --exists-action i);
    push( @args, ( &do_update ? '-U' : '--disable-pip-version-check' ) );
    run( $pip2_exec, @args, @pip2_install_intermediate );
}

my sub pip3_install_reducer {
    push( @pip3_install_intermediate, 'pip' ) if (&do_update);
    return if ( scalar(@pip3_install_intermediate) eq 0 );

    log_wait('Installing Python3 packages ...');

    my $pip3_exec = &find_pip3_exec;
    error('pip3 is not found.') unless ( defined($pip3_exec) );

    my @args = qw(install --exists-action i);
    push( @args, ( &do_update ? '-U' : '--disable-pip-version-check' ) );
    run( $pip3_exec, @args, @pip3_install_intermediate );
}

register_reducer( 61, \&pip2_install_reducer );
register_reducer( 61, \&pip3_install_reducer );
