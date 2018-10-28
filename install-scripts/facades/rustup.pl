# 50_rustup.pl --- rustup facade
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

my @rustup_component_add_intermediate = ();

sub rustup_component_add {

    # TODO Enable to specify a Rust toolchain
    my $component = $_[0];

    push( @rustup_component_add_intermediate, $component );
}

my sub rustup_component_add_reducer {
    return if ( scalar(@rustup_component_add_intermediate) eq 0 );

    log_wait('Adding Rust toolchain components ...');

    # TODO Install rustup
    # TODO Install Rust toolchains: stable, nightly
    error('rustup not found.') unless ( is_exec('rustup') );

    # TODO Skip update checking unless --update given
    run_cmd( qw(rustup component add), @rustup_component_add_intermediate );
}

register_reducer( 20, \&rustup_component_add_reducer );

1;
