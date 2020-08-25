{ lib, ... }:

with lib; {
  imports = [ ./alfred.nix ./notion.nix ];
}
