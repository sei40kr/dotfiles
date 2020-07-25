{ lib, ... }:

with lib; {
  imports = [ ./tools ./tmux.nix ./zsh.nix ];
}
