# defaults.pl --- defaults facade
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

my @defaults_write_intermediate = ();

sub defaults_write_bool {
    my ( $domain, $key, $value ) = @_;

    push(
        @defaults_write_intermediate,
        {
            type   => 'bool',
            domain => $domain,
            key    => $key,
            value  => $value
        }
    );
}

sub defaults_write_int {
    my ( $domain, $key, $value ) = @_;

    push(
        @defaults_write_intermediate,
        {
            type   => 'int',
            domain => $domain,
            key    => $key,
            value  => $value
        }
    );
}

sub defaults_write_float {
    my ( $domain, $key, $value ) = @_;

    push(
        @defaults_write_intermediate,
        {
            type   => 'float',
            domain => $domain,
            key    => $key,
            value  => $value
        }
    );
}

sub defaults_write_string {
    my ( $domain, $key, $value ) = @_;

    push(
        @defaults_write_intermediate,
        {
            type   => 'string',
            domain => $domain,
            key    => $key,
            value  => $value
        }
    );
}

sub defaults_write_array {
    my ( $domain, $key, @values ) = @_;

    push(
        @defaults_write_intermediate,
        {
            type   => 'array',
            domain => $domain,
            key    => $key,
            values => \@values
        }
    );
}

my sub defaults_write_reducer {
    return if ( scalar(@defaults_write_intermediate) eq 0 );

    log_wait('Writing the macOS user defaults ...');

    unless (&is_macos) {
        error('Accessing the macOS user defaults is available only on macOS.');
    }

    # Close any open System Preferences panes, to prevent them from overriding
    # settings we’re about to change
    run_cmd( qw(osascript -e),
        'tell application "System Preferences" to quit' );

    foreach my $item (@defaults_write_intermediate) {
        my @defaults_args =
          ( 'write', $item->{domain}, $item->{key}, '-' . $item->{type} );
        if ( $item->{type} eq 'array' ) {
            push( @defaults_args, @{ $item->{values} } );
        }
        elsif ( $item->{type} eq 'bool' ) {
            push( @defaults_args, $item->{value} ? "true" : "false" );
        }
        else {
            push( @defaults_args, $item->{value} );
        }

        run_cmd( 'defaults', @defaults_args );
    }
}

register_reducer( \&defaults_write_reducer );

1;
