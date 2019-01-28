# 50_go.pl --- go facade
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

my @go_get_intermediate = ();

sub go_get {
    my $pkg = $_[0];

    push( @go_get_intermediate, $pkg );
}

my sub find_go_exec {
    foreach my $dirpath ("${ENV{HOME}}/.goenv/shims", "/usr/local/bin", "/usr/bin") {
        return "${dirpath}/go" if (-x "${dirpath}/go");
    }
}

my sub go_get_reducer {
    return if ( scalar(@go_get_intermediate) eq 0 );

    my $go_exec = &find_go_exec;
    error('go is not installed') unless (defined($go_exec));

    log_wait('Installing Go packages ...');

    my @go_args = ('get');
    push( @go_args, '-u' ) if (&do_update);
    Command::run( $go_exec, @go_args, @go_get_intermediate );
}

register_reducer( 61, \&go_get_reducer );

1;
