{ config, lib, ... }:

with lib;
with lib.my; {
  config = {
    user.home = "/Users/${config.user.name}";

    # Necessary for home-manager to work with flakes, otherwise it will
    # look for a nixpkgs channel.
    home-manager.users.${config.user.name}.home.stateVersion =
      config.system.nixpkgsRelease;
  };
}
