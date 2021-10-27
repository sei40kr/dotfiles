{ config, lib, ... }:

with lib;
with lib.my; {
  config = {
    user = {
      extraGroups = [ "wheel" ];
      group = "users";
      home = "/home/${config.user.name}";
      isNormalUser = true;
    };

    # Necessary for home-manager to work with flakes, otherwise it will
    # look for a nixpkgs channel.
    home-manager.users.${config.user.name}.home = {
      inherit (config.system) stateVersion;
    };
  };
}
