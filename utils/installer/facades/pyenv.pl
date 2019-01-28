# pyenv.pl --- pyenv facade
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

my @pyenv_install_intermediate = ();
my @pyenv_global_intermediate  = ();

sub pyenv_install {
    my $version = $_[0];

    push( @pyenv_install_intermediate, $version );
}

sub pyenv_global {
    @pyenv_global_intermediate = @_;
}

my sub find_pyenv_exec {
    foreach
      my $dirpath ( "${ENV{HOME}}/.pyenv/bin", "/usr/local/bin", "/usr/bin" )
    {
        return "${dirpath}/pyenv" if ( -x "${dirpath}/pyenv" );
    }
}

my sub pyenv_install_reducer {
    return if ( scalar(@pyenv_install_intermediate) eq 0 );

    log_wait('Installing Python ...');

    my $pyenv_exec = &find_pyenv_exec;
    error('pyenv is not installed.') unless ( defined($pyenv_exec) );

    Command::run( $pyenv_exec, 'install', '-s', $_ )
      foreach @pyenv_install_intermediate;
}

my sub pyenv_global_reducer {
    return if ( scalar(@pyenv_global_intermediate) eq 0 );

    log_wait('Setting the global Python versions ...');

    my $pyenv_exec = &find_pyenv_exec;
    error('pyenv is not installed.') unless ( defined($pyenv_exec) );

    Command::run( $pyenv_exec, 'global', @pyenv_global_intermediate );
}

register_reducer( 50, \&pyenv_install_reducer );
register_reducer( 51, \&pyenv_global_reducer );

1;
