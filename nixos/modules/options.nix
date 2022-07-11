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
  };
}
