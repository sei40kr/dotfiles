# 30_ln.pl --- ln facade
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use File::Basename qw(dirname);
use File::Path qw(mkpath);

my @ln_intermediate = ();

sub ln {
    my ( $src, $dest ) = @_;

    push(
        @ln_intermediate,
        {
            src  => $src,
            dest => $dest,
        }
    );
}

my sub ln_reducer {
    return if ( scalar(@ln_intermediate) eq 0 );

    log_wait('Creating symlinks ...');

    foreach my $item (@ln_intermediate) {

        # Call symlink instead of running `ln` for better performance
        if ( &is_dry_run || &is_verbose ) {
            printf "> mkpath('%s');\n", dirname( $item->{dest} );
            printf "> symlink '%s', '%s';\n", $item->{src}, $item->{dest};
        }
        unless (&is_dry_run) {
            mkpath( dirname( $item->{dest} ) );
            symlink $item->{src}, $item->{dest};
        }
    }
}

register_reducer( 30, \&ln_reducer );

1;
