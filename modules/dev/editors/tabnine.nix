{ config, lib, options, pkgs, ... }:

with lib; {
  config.my.home.xdg.configFile."TabNine/TabNine.toml".source =
    <config/tabnine/TabNine.toml>;
}
