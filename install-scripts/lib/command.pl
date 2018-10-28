# command.pl
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

package Command;

sub run {
    my @command = @_;

    print '> ' . join( ' ', @command ) . "\n"
      if ( &main::is_dry_run or &main::is_verbose );

    unless (&main::is_dry_run) {
        my $proc;
        open $proc, '-|', @command;
        if (&main::is_verbose) {
            print while (<$proc>);
        }
        close $proc;
    }
}

1;
