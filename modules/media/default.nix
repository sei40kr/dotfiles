{ lib, ... }:

with lib; {
  imports = [ ./eog.nix ./totem.nix ];
}
