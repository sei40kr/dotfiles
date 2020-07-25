{ lib, ... }:

with lib; {
  imports = [ ./fontconfig.nix ./gtk.nix ];
}
