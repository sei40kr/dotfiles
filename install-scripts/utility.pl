# utility.pl --- dotfiles installer utility
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use Term::ANSIColor;

my %options = (
    dry_run => 0,
    update  => 0,
    verbose => 0,
);
my @reducers = ();
my @exec_paths = split( ":", $ENV{PATH} );

sub set_dry_run {
    my $value = $_[0];

    $options{dry_run} = $value;
}

sub set_update {
    my $value = $_[0];

    $options{update} = $value;
}

sub set_verbose {
    my $value = $_[0];

    $options{verbose} = $value;
}

sub is_macos {
    return $^O eq 'darwin';
}

sub is_linux {
    return $^O eq 'linux';
}

sub is_arch {
    return ( -f '/etc/arch-release' );
}

sub log_wait {
    my $msg = $_[0];

    printf "%s %s\n", colored( '==>', 'blue' ), colored( $msg, 'bold' );
}

sub error {
    my ( $msg, $errcode ) = @_;
    $errcode = 1 unless ( defined($errcode) );

    printf "%s %s\n", colored( 'ERROR:', 'red' ), $msg;
    exit $errcode;
}

sub run_reducers {
    foreach my $reducer (@reducers) {
        &{$reducer};
    }
}

sub is_dry_run {
    return $options{dry_run};
}

sub do_update {
    return $options{update};
}

sub is_verbose {
    return $options{verbose};
}

sub is_exec {
    my $cmd = $_[0];

    foreach my $path (@exec_paths) {
        return 1 if ( -e "${path}/${cmd}" );
    }

    return 0;
}

sub run_cmd {
    my @cmd = @_;

    if ( &is_dry_run or &is_verbose ) {
        print( '> ', join( ' ', @cmd ), "\n" );
    }

    unless (&is_dry_run) {
        my $proc;
        my $verbose = &is_verbose;
        open $proc, '-|', @cmd;
        while (<$proc>) {
            print if ($verbose eq 1);
        };
        close $proc or return 0;
    }

    return 1;
}

sub register_reducer {
    my $reducer = $_[0];

    push( @reducers, $reducer );
}

1;
