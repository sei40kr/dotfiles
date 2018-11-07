# Logger.pm
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use Term::ANSIColor;

sub log_warn {
    my $message = $_[0];

    printf "%s %s\n", colored( 'WARN:', 'yellow' ), $message;
}

sub log_trace {
    my $to_trace = $_[0];

    # TODO Trim an blank line at last
    print "> $to_trace\n"
}

1;
