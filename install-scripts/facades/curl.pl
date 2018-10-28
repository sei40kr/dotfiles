# curl.pl --- curl facade
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

my @curl_intermediate = ();

sub curl {
    my ( $url, $dest ) = @_;

    push(
        @curl_intermediate,
        {
            url  => $url,
            dest => $dest
        }
    );
}

my sub curl_reducer {
    my @curl_args = ();
    foreach my $item (@curl_intermediate) {
        push( @curl_args, '-o', $item->{dest}, '--create-dirs', $item->{url} )
          if ( &do_update or !-e $item->{dest} );
    }

    return if ( scalar(@curl_args) eq 0 );

    log_wait('Transferring data from a server ...');

    error('curl not found') unless ( is_exec('curl') );

    Command::run( 'curl', qw(-SsLK /dev/null), @curl_args );
}

register_reducer( 30, \&curl_reducer );

1;
