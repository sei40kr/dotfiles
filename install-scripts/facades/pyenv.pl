# pyenv.pl --- pyenv facade
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

my $pyenv_executable_path = "${ENV{PYENV_ROOT}}/bin/pyenv";

my @pyenv_install_intermediate = ();

sub pyenv_install {
    my $version = $_[0];

    push( @pyenv_install_intermediate, $version );
}

my sub pyenv_check_executable {
    error('pyenv not found.')
      unless ( -x $pyenv_executable_path || &is_dry_run );
}

my sub pyenv_install_reducer {
    return if ( scalar(@pyenv_install_intermediate) eq 0 );

    &pyenv_check_executable;

    log_wait('Installing Python ...');

    my @args = ('install');
    push( @args, &do_update ? '-f' : '-s' );
    Command::run( $pyenv_executable_path, @args, $_ )
      foreach @pyenv_install_intermediate;
}

register_reducer( 50, \&pyenv_install_reducer );

1;
