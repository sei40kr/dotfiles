# 50_rustup.pl --- rustup facade
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

my %rustup_component_add_intermediate = ();

sub rustup_component_add {
    my ( $component, $toolchain ) = @_;
    $toolchain = 'stable' unless ( defined($toolchain) );

    $rustup_component_add_intermediate{$toolchain} = []
      unless ( defined( $rustup_component_add_intermediate{$toolchain} ) );
    push( @{ $rustup_component_add_intermediate{$toolchain} }, $component );
}

my sub rustup_component_add_reducer {
    return if ( scalar( ( keys %rustup_component_add_intermediate ) ) eq 0 );

    log_wait('Adding Rust toolchain components ...');

    error('rustup not found.') unless ( is_exec('rustup') );

    foreach my $toolchain ( keys %rustup_component_add_intermediate ) {

        # TODO Skip update checking unless --update given
        Command::run( qw(rustup component add --toolchain),
                      $toolchain, @{ $rustup_component_add_intermediate{$toolchain} } );
    }
}

register_reducer( 60, \&rustup_component_add_reducer );

1;
