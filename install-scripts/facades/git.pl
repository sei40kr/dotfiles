# 20_git.pl --- Git facade
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

my @git_clone_intermediate = ();

sub git_clone {
    my ( $repo, $dest, $branch ) = @_;
    $branch = 'master' unless ( defined($branch) );

    push(
        @git_clone_intermediate,
        {
            repo   => $repo,
            dest   => $dest,
            branch => $branch
        }
    );
}

my sub git_clone_reducer() {
    return if ( scalar(@git_clone_intermediate) eq 0 );

    log_wait('Cloning repositories ...');

    git_clone_internal( $_->{repo}, $_->{branch}, $_->{dest} )
      foreach @git_clone_intermediate;
}

register_reducer( 20, \&git_clone_reducer );

1;
