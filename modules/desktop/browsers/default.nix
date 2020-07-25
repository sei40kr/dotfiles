{ lib, ... }:

with lib; {
  imports = [ ./chromium.nix ./qutebrowser.nix ];
}
