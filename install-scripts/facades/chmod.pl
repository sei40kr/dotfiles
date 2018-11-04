# chmod.pl
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

my @chmod_intermediate = ();

sub chmod_facade {
    my ( $mode, $file ) = @_;

    push(
        @chmod_intermediate,
        {
            mode => $mode,
            file => $file,
        }
    );
}

my sub chmod_reducer {
    return if ( scalar(@chmod_intermediate) eq 0 );

    log_wait('Changing file mode ...');

    foreach my $item (@chmod_intermediate) {

        if ( &is_dry_run || &is_verbose ) {
            printf "> chmod %04d, '%s';\n", $item->{mode}, $item->{file};
        }
        unless (&is_dry_run) {
            chmod $item->{mode}, $item->{file};
        }
    }
}

register_reducer( 70, \&chmod_reducer );

1;
