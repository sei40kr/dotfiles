# goenv.pl --- goenv facade
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/utils/installer/lib";
use Install::CommandRunner;

my @goenv_install_intermediate = ();
my $goenv_global_intermediate;

sub goenv_install {
    my $version = $_[0];

    push( @goenv_install_intermediate, $version );
}

sub goenv_global {
    $goenv_global_intermediate = $_[0];
}

my sub find_goenv_exec {
    foreach
      my $dirpath ( "${ENV{HOME}}/.goenv/bin", "/usr/local/bin", "/usr/bin" )
    {
        return "${dirpath}/goenv" if ( -x "${dirpath}/goenv" );
    }
}

my sub goenv_install_reducer {
    return if ( scalar(@goenv_install_intermediate) eq 0 );

    log_wait('Installing Go ...');

    my $goenv_exec = &find_goenv_exec;
    error('goenv is not installed.') unless ( defined($goenv_exec) );

    run( $goenv_exec, 'install', '-s', $_ ) foreach @goenv_install_intermediate;
}

my sub goenv_global_reducer {
    return unless ( defined($goenv_global_intermediate) );

    log_wait('Setting the global Go version ...');

    my $goenv_exec = &find_goenv_exec;
    error('goenv is not installed.') unless ( defined($goenv_exec) );

    run( $goenv_exec, 'global', $goenv_global_intermediate );
}

register_reducer( 50, \&goenv_install_reducer );
register_reducer( 51, \&goenv_global_reducer );

1;
