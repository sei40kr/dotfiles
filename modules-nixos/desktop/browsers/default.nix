{ lib, ... }:

with lib; {
  imports = [ ./chromium.nix ./firefox.nix ./qutebrowser.nix ];
}
