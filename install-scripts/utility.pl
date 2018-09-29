# utility.pl --- dotfiles installer utility
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use File::Basename qw(dirname);
use File::Path qw(rmtree);
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

sub exec_path {
    my $cmd = $_[0];

    foreach my $path (@exec_paths) {
        return $path if ( -e "${path}/${cmd}" );
    }

    return undef;
}

sub is_exec {
    my $cmd = $_[0];

    return substr( $cmd, 0, 1 ) eq '/' ? ( -x $cmd ) : ( -x exec_path($cmd) );
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
            print if ( $verbose eq 1 );
        }
        close $proc or return 0;
    }

    return 1;
}

my sub git_get_url {
    my ( $path, $remote ) = @_;

    my $url = `git -C "${path}" remote get-url ${remote} 2>/dev/null`;
    if ( $? ne 0 ) {
        return undef;
    }
    else {
        chomp $url;
        return $url;
    }
}

my sub git_current_branch {
    my $path = $_[0];
    my $branch;

    open PROC, '-|', qw(git -C), $path, qw(branch);
    while ( my $line = <PROC> ) {
        if ( substr( $line, 0, 2 ) eq '* ' ) {
            $branch = substr $line, 2;
            chomp $branch;
            last;
        }
    }
    close PROC;

    return $branch;
}

sub git_clone_internal {
    my ( $repo, $branch, $dest ) = @_;

    my $remote_url = git_get_url( $dest, 'origin' );
    unless ( defined($remote_url) and $remote_url eq $repo ) {
        printf "> rmtree('%s');\n", $dest if ( &is_dry_run or &is_verbose );
        rmtree($dest) unless (&is_dry_run);

        my $parent_dir = dirname($dest);
        printf "> mkpath('%s');\n", $parent_dir
          if ( &is_dry_run or &is_verbose );
        mkpath($parent_dir) unless (&is_dry_run);

        run_cmd( qw(git clone -q --recurse-submodules -b),
            $branch, $repo, $dest );
    }
    elsif ( &do_update and git_current_branch($dest) eq $branch ) {
        run_cmd(qw(git pull -q --recurse-submodules=yes --ff-only -r true));
    }
}

sub register_reducer {
    my $reducer = $_[0];

    push( @reducers, $reducer );
}

1;
