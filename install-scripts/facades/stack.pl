# 50_stack.pl --- Stack facade
# author: Seong Yong-ju <sei40kr@gmail.com>

my @stack_install_intermediate = ();

sub stack_install {
    my ( $pkg, @flags ) = @_;

    push(
        @stack_install_intermediate,
        {
            pkg   => $pkg,
            flags => \@flags
        }
    );
}

my sub stack_install_reducer {
    return if ( scalar(@stack_install_intermediate) eq 0 );

    log_wait('Installing Stack packages ...');

    error('stack not found.') unless ( is_exec('stack') );

    @stack_pkgs         = ();
    @stack_install_opts = ();
    foreach my $item (@stack_install_intermediate) {
        my $pkg = $item->{pkg};
        push( @stack_pkgs, $pkg );
        push( @stack_install_opts, ( '--flag', "${pkg}:${_}" ) )
          foreach @{ $item->{flags} };
    }
    push( @stack_args, $_->{pkg} ) foreach @stack_install_intermediate;

    run_cmd( qw(stack install), @stack_pkgs, @stack_install_opts );
}

register_reducer( 20, \&stack_install_reducer );

1;
