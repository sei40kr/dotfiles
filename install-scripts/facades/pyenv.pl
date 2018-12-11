# pyenv.pl --- pyenv facade
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

my $pyenv_executable_path = "${ENV{PYENV_ROOT}}/bin/pyenv";

my @pyenv_install_intermediate = ();
my @pyenv_global_intermediate  = ();

sub pyenv_install {
    my $version = $_[0];

    push( @pyenv_install_intermediate, $version );
}

sub pyenv_global {
    my ( $version3, $version2 ) = @_;

    @pyenv_global_intermediate = ( $version3, $version2 );
}

my sub pyenv_check_existence {
    error('pyenv not found.')
      unless ( -x $pyenv_executable_path || &is_dry_run );
}

my sub pyenv_install_reducer {
    return if ( scalar(@pyenv_install_intermediate) eq 0 );

    &pyenv_check_existence;

    log_wait('Installing Python ...');

    my @args = ('install');
    push( @args, &do_update ? '-f' : '-s' );
    Command::run( $pyenv_executable_path, @args, $_ )
      foreach @pyenv_install_intermediate;
}

my sub pyenv_global_reducer() {
    my ( $version3, $version2 ) = @pyenv_global_intermediate;
    return if ( !defined($version3) && !defined($version2) );

    &pyenv_check_existence;

    log_wait('Setting the global Python versions ...');

    my @args = ( 'global', $version3 );
    push( @args, $version2 ) if ( defined($version2) );
    Command::run( $pyenv_executable_path, @args );
}

register_reducer( 50, \&pyenv_install_reducer );
register_reducer( 51, \&pyenv_global_reducer );

1;
