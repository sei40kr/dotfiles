# 50_cargo.pl --- cargo facade
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

my sub cargo_install_reducer {
    return if ( scalar(@cargo_install_intermediate) eq 0 );

    log_wait('Installing Rust binaries ...');

    my $cargo = "${ENV{CARGO_HOME}}/bin/cargo";

    # TODO Install rustup
    error('cargo not found.') unless ( is_exec($cargo) );

    log_warn("cargo can't skip checking package updates.") unless (&do_update);

    Command::run( $cargo, qw(install -fq), @cargo_install_intermediate );
}

my sub cargo_nightly_install_reducer {
    return if ( scalar(@cargo_nightly_install_intermediate) eq 0 );

    log_wait('Installing Rust binaries ...');

    my $cargo = "${ENV{CARGO_HOME}}/bin/cargo";

    error('cargo not found.') unless ( is_exec($cargo) );

    log_warn("cargo can't skip checking package updates.") unless (&do_update);

    Command::run(
        $cargo,
        qw(+nightly install -fq),
        @cargo_nightly_install_intermediate
    );
}

register_reducer( 61, \&cargo_install_reducer );
register_reducer( 61, \&cargo_nightly_install_reducer );

1;
