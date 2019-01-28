# rbenv.pl --- rbenv facade
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/utils/installer/lib";
use Install::CommandRunner;

my @rbenv_install_intermediate = ();
my $rbenv_global_intermediate;

sub rbenv_install {
    my $version = $_[0];

    push( @rbenv_install_intermediate, $version );
}

sub rbenv_global {
    $rbenv_global_intermediate = $_[0];
}

my sub find_rbenv_exec {
    foreach
      my $dirpath ( "${ENV{HOME}}/.rbenv/bin", "/usr/local/bin", "/usr/bin" )
    {
        return "${dirpath}/rbenv" if ( -x "${dirpath}/rbenv" );
    }
}

my sub rbenv_install_reducer {
    return if ( scalar(@rbenv_install_intermediate) eq 0 );

    log_wait('Installing Ruby ...');

    my $rbenv_exec = &find_rbenv_exec;
    error('rbenv is not installed.') unless ( defined($rbenv_exec) );

    run( $rbenv_exec, 'install', '-s', $_ )
      foreach @rbenv_install_intermediate;
}

my sub rbenv_global_reducer {
    return unless ( defined($rbenv_global_intermediate) );

    log_wait('Setting the global Ruby version ...');

    my $rbenv_exec = &find_rbenv_exec;
    error('rbenv is not installed.') unless ( defined($rbenv_exec) );

    run( $rbenv_exec, 'global', $rbenv_global_intermediate );
}

register_reducer( 50, \&rbenv_install_reducer );
register_reducer( 51, \&rbenv_global_reducer );

1;
