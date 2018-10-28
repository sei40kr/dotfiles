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

sub run_pipe {
    my ( $left_command, $right_command ) = @_;

    print '> '
      . join( ' ', @{$left_command} ) . ' | '
      . join( '',  @{$right_command} ) . "\n"
      if ( &main::is_dry_run or &main::is_verbose );

    unless (&main::is_dry_run) {
        my $left_proc;
        my $right_proc;
        open $left_proc,  '-|', @{$left_command};
        open $right_proc, '|-', @{$right_command};
        print $right_proc while (<$left_proc>);
        if (&main::is_verbose) {
            print while (<$right_proc>);
        }
        close $right_proc;
        close $left_proc;
    }
}

sub run_with_stdin {
    my ( $stdin, @command ) = @_;

    if ( &main::is_dry_run or &main::is_verbose ) {
        my @lines = split(/\n/, $stdin);
        pop @lines if ($lines[-1] eq '');

        print '> ' . join( ' ', @command ) . " <<EOM\n";
        print "> ${_}\n" foreach @lines;
        print "> EOM\n"
    }

    unless (&main::is_dry_run) {
        my $proc;
        open $proc, '|-', @command;
        print $proc $stdin;
        if (&main::is_verbose) {
            print while (<$proc>);
        }
        close $proc;
    }
}

1;
