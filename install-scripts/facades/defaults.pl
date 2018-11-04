# defaults.pl --- defaults facade
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

my %defaults_write_intermediate = ();

my sub defaults_write($$$$) {
    my ( $domain, $key, $type, $value ) = @_;

    unless ( defined( $defaults_write_intermediate{$domain} ) ) {
        $defaults_write_intermediate{$domain} = [];
    }

    push(
        @{ $defaults_write_intermediate{$domain} },
        {
            key   => $key,
            type  => $type,
            value => $value
        }
    );
}

sub defaults_write_bool {
    my ( $domain, $key, $value ) = @_;

    defaults_write( $domain, $key, 'bool', $value );
}

sub defaults_write_int {
    my ( $domain, $key, $value ) = @_;

    defaults_write( $domain, $key, 'int', $value );
}

sub defaults_write_float {
    my ( $domain, $key, $value ) = @_;

    defaults_write( $domain, $key, 'float', $value );
}

sub defaults_write_string {
    my ( $domain, $key, $value ) = @_;

    defaults_write( $domain, $key, 'string', $value );
}

my sub generate_plist($) {
    my $domain = $_[0];
    my $s      = <<EOM;
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
    "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
EOM

    foreach my $item ( @{ $defaults_write_intermediate{$domain} } ) {
        $s .= '<key>' . $item->{key} . "</key>\n";

        if ( $item->{type} eq 'bool' ) {
            $s .= $item->{value} ? "<true/>\n" : "<false/>\n";
        }
        elsif ( $item->{type} eq 'float' ) {
            $s .= '<real>' . $item->{value} . "</real>\n";
        }
        elsif ( $item->{type} eq 'int' ) {
            $s .= '<integer>' . $item->{value} . "</integer>\n";
        }
        elsif ( $item->{type} eq 'string' ) {
            $s .= '<string>' . $item->{value} . "</string>\n";
        }
    }

    $s .= <<EOM;
</dict>
</plist>
EOM

    return $s;
}

my sub defaults_write_reducer() {
    return if ( scalar( keys %defaults_write_intermediate ) eq 0 );

    log_wait('Writing the macOS user defaults ...');

    unless (&is_macos) {
        error('Accessing the macOS user defaults is available only on macOS.');
    }

    # Close any open System Preferences panes, to prevent them from overriding
    # settings we’re about to change
    # Command::run( qw(osascript -e),
    #     'tell application "System Preferences" to quit' );

    foreach my $domain ( keys %defaults_write_intermediate ) {
        my $plist = &generate_plist($domain);
        my @command = ( 'defaults', 'import', $domain, '-' );
        Command::run_with_stdin( $plist, @command );
    }
}

register_reducer( 30, \&defaults_write_reducer );

1;
