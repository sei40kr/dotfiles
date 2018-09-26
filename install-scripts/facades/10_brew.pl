# 10_brew.pl --- brew facade
# author: Seong Yong-ju <sei40kr@gmail.com>

my %brew_intermediate = {
    repos              => (),
    formulas_with_opts => (),
    casks              => (),
};

sub brew_tap() {
    my ( $repo, $url ) = @_;

    push(
        @{ $brew_intermediate{repos} },
        {
            repo => $repo,
            url  => $url
        }
    );
}

sub brew_install() {
    my ( $formula, @opts ) = @_;

    push(
        @{ $brew_intermediate{formulas_with_opts} },
        {
            formula => $formula,
            opts    => \@opts
        }
    );
}

sub brew_cask_install() {
    my $cask = $_[0];

    push( @{ $brew_intermediate{casks} }, $cask );
}

my sub dump_brewfile {
    my $s = '';

    foreach my $repo ( @{ $brew_intermediate{repos} } ) {
        if ( defined( $repo->{url} ) ) {
            $s .=
              sprintf( "tap \"%s\", \"%s\"\n", $repo->{repo}, $repo->{url} );
        }
        else {
            $s .= sprintf( "tap \"%s\"\n", $repo->{repo} );
        }

    }

    foreach
      my $formula_with_opts ( @{ $brew_intermediate{formulas_with_opts} } )
    {
        $s .= sprintf(
            "brew \"%s\", args: [%s]\n",
            $formula_with_opts->{formula},
            join( ', ', map { "'$_'" } @{ $formula_with_opts->{opts} } )
        );
    }

    if ( $brew_intermediate{casks} ne 0 ) {
        $s .= "cask_args appdir: \"~/Applications\"\n";
    }
    foreach my $cask ( @{ $brew_intermediate{casks} } ) {
        $s .= "cask \"${cask}\"\n";
    }

    return $s;
}

sub brew_reducer() {
    if (    $brew_intermediate{repos} eq 0
        and $brew_intermediate{formulas} eq 0
        and $brew_intermediate{casks} eq 0 )
    {
        return;
    }

    my @cmd = qw( brew bundle --file=- );
    # TODO Check updates if --update option specified
    push( @cmd, "--no-upgrade" );

    open( BREW, '|-', @cmd );

    print BREW dump_brewfile();

    while (<BREW>) { print; }

    close BREW;
}
