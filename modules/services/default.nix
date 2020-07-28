{ lib, ... }:

with lib; {
  imports = [ ./flexget.nix ./jellyfin.nix ./rclone.nix ];
}
