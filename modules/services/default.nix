{ lib, ... }:

with lib; {
  imports = [ ./cupsd.nix ./flexget.nix ./jellyfin.nix ./rclone.nix ];
}
