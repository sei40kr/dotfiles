# 30_cp.pl --- cp facade
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use File::Basename qw(dirname);
use File::Copy qw(copy);
use File::Path qw(mkpath);

my @cp_intermediate = ();

sub cp {
    my ( $src, $dest ) = @_;

    push(
        @cp_intermediate,
        {
            src  => $src,
            dest => $dest,
        }
    );
}

my sub cp_reducer {
    return if ( scalar(@cp_intermediate) eq 0 );

    log_wait('Copying files and directories ...');

    foreach my $item (@cp_intermediate) {
        if ( &is_dry_run || &is_verbose ) {

            # TODO Use log_trace
            printf "> mkpath('%s');\n", dirname( $item->{dest} );
            printf "> copy('%s', '%s');\n", $item->{src}, $item->{dest};
        }
        unless (&is_dry_run) {
            mkpath( dirname( $item->{dest} ) );
            copy( $item->{src}, $item->{dest} );
        }
    }
}

register_reducer( 70, \&cp_reducer );

1;
