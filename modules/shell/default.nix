{ lib, ... }:

with lib; {
  imports = [ ./tools ./tmux.nix ./xonsh.nix ./zsh.nix ];
}
