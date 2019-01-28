# curl.pl --- curl facade
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/utils/installer/lib";
use Install::CommandRunner;

my @curl_intermediate    = ();
my @curl_sh_intermediate = ();

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

sub curl_sh {
    my $url = $_[0];

    push( @curl_intermediate, $url );
}

my sub curl_existence_check {
    error('curl not found') unless ( is_exec('curl') );
}

my sub curl_reducer {
    my @curl_args = ();
    foreach my $item (@curl_intermediate) {
        push( @curl_args, '-o', $item->{dest}, '--create-dirs', $item->{url} )
          if ( &do_update or !-e $item->{dest} );
    }

    return if ( scalar(@curl_args) eq 0 );

    log_wait('Transferring data from a server ...');

    &curl_existence_check;

    run( 'curl', qw(-SsLK /dev/null), @curl_args );
}

my sub curl_sh_reducer {
    return if ( scalar(@curl_sh_intermediate) eq 0 );

    log_wait('Transferring data from a server ...');

    &curl_existence_check;

    run_pipe( [ qw(curl -SsLK /dev/null), $_->{url} ], ['sh'] )
      foreach @curl_sh_intermediate;
}

register_reducer( 20, \&curl_reducer );
register_reducer( 30, \&curl_sh_reducer );

1;
