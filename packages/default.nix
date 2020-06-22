[
  (self: super:
    with super; {
      unstable = import <nixos-unstable> { inherit config; };
    })

  (import (builtins.fetchTarball {
    url =
      "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
  }))
]
