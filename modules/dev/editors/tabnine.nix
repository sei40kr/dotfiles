{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.editors.tabnine.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.editors.tabnine.enable {
    my.home.xdg.configFile."TabNine/TabNine.toml".source =
      <config/tabnine/TabNine.toml>;
  };
}
