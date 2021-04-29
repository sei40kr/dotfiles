{ config, lib, ... }:

with lib;
with lib.my; {
  config.home-manager.users.${config.user.name}.xdg = {
    enable = true;
    cacheHome = "${config.user.home}/.cache";
    configHome = "${config.user.home}/.config";
    dataHome = "${config.user.home}/.local/share";
  };
}
