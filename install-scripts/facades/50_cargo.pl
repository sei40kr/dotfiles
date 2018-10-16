# 50_cargo.pl --- cargo facade
# author: Seong Yong-ju <sei40kr@gmail.com>

my @cargo_install_intermediate = ();

sub cargo_install {
    my $pkg = $_[0];

    push( @cargo_install_intermediate, $pkg );
}

my sub cargo_install_reducer {
    return if ( scalar(@cargo_install_intermediate) eq 0 );

    log_wait('Installing Rust binaries ...');

    my $cargo = "${ENV{CARGO_HOME}}/bin/cargo";
    error('cargo not found.') unless ( is_exec($cargo) );

    @cargo_args = qw(install -fq);
    push( @cargo_args, @cargo_install_intermediate );

    run_cmd( $cargo, @cargo_args );
}

register_reducer( \&cargo_install_reducer );

1;
