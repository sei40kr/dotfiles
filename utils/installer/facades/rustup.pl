# 50_rustup.pl --- rustup facade
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/utils/installer/lib";
use Install::CommandRunner;
use Install::Logger;

my @rustup_toolchain_install_intermediate = ();
my %rustup_component_add_intermediate     = ();

sub rustup_toolchain_install {
    my $toolchain = $_[0];

    push( @rustup_toolchain_install_intermediate, $toolchain );
}

sub rustup_component_add {
    my ( $component, $toolchain ) = @_;
    $toolchain = 'stable' unless ( defined($toolchain) );

    $rustup_component_add_intermediate{$toolchain} = []
      unless ( defined( $rustup_component_add_intermediate{$toolchain} ) );
    push( @{ $rustup_component_add_intermediate{$toolchain} }, $component );
}

my sub find_rustup_exec {
    foreach my $dirpath ("${ENV{HOME}}/.cargo/bin") {
        return "${dirpath}/rustup" if ( -x "${dirpath}/rustup" );
    }
}

my sub rustup_toolchain_install_reducer {
    return if ( scalar( (@rustup_toolchain_install_intermediate) ) eq 0 );

    log_wait('Installing Rust toolchains ...');

    my $rustup_exec = &find_rustup_exec;
    error('rustup is not installed.') unless ( defined($rustup_exec) );

    log_warn(
"rustup can't skip checking updates for these Rust versions: stable, beta, and nightly."
      )
      if ( !&do_update
        && grep( /^stable|beta|nightly$/,
            @rustup_toolchain_install_intermediate ) );
    run( $rustup_exec, 'toolchain', 'install', $_ )
      foreach @rustup_toolchain_install_intermediate;
}

my sub rustup_component_add_reducer {
    return if ( scalar( ( keys %rustup_component_add_intermediate ) ) eq 0 );

    log_wait("Adding the Rust toolchains' components ...");

    my $rustup_exec = &find_rustup_exec;
    error('rustup is not installed.') unless ( defined($rustup_exec) );

    log_warn("rustup can't skip checking toolchains' component updates.")
      unless (&do_update);

    foreach my $toolchain ( keys %rustup_component_add_intermediate ) {
        run( $rustup_exec, 'component', 'add', '--toolchain',
            $toolchain, @{ $rustup_component_add_intermediate{$toolchain} } );
    }
}

register_reducer( 50, \&rustup_toolchain_install_reducer );
register_reducer( 60, \&rustup_component_add_reducer );

1;
