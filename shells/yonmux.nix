{ mkShell, my }:

mkShell {
  buildInputs = [ my.yonmux ];

  shellHook = ''
    export TMUX=
    export TMUX_TMPDIR=$(mktemp -d)

    _cleanup() {
      rm -rf $TMUX_TMPDIR
    }

    trap _cleanup EXIT
  '';
}
