# cargo.pl --- Cargo facade
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/install-scripts/lib";
use InstallHelper::Logger;

my @cargo_install_intermediate         = ();
my @cargo_nightly_install_intermediate = ();

sub cargo_install {
    my $pkg = $_[0];

    push( @cargo_install_intermediate, $pkg );
}

sub cargo_nightly_install {
    my $pkg = $_[0];

    push( @cargo_nightly_install_intermediate, $pkg );
}

my sub find_cargo_exec {
    foreach
      my $dirpath ( "${ENV{HOME}}/.cargo/bin", "/usr/local/bin", "/usr/bin" )
    {
        return "${dirpath}/cargo" if ( -x "${dirpath}/cargo" );
    }
}

my sub cargo_install_reducer {
    return if ( scalar(@cargo_install_intermediate) eq 0 );

    log_wait('Installing Rust binaries ...');

    my $cargo_exec = &find_cargo_exec;
    error('Cargo is not installed.') unless ( defined($cargo_exec) );

    log_warn("Cargo can't skip checking package updates.") unless (&do_update);

    Command::run( $cargo_exec, 'install', '-fq', @cargo_install_intermediate );
}

my sub cargo_nightly_install_reducer {
    return if ( scalar(@cargo_nightly_install_intermediate) eq 0 );

    log_wait('Installing nightly Rust binaries ...');

    my $cargo_exec = &find_cargo_exec;
    error('Cargo is not installed.') unless ( defined($cargo_exec) );

    log_warn("Cargo can't skip checking package updates.") unless (&do_update);

    Command::run( $cargo_exec, '+nightly', 'install', '-fq',
        @cargo_nightly_install_intermediate );
}

register_reducer( 61, \&cargo_install_reducer );
register_reducer( 61, \&cargo_nightly_install_reducer );

1;
