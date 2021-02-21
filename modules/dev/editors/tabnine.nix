{ config, lib, ... }:

with lib;
with lib.my; {
  options.modules.dev.editors.tabnine.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.editors.tabnine.enable {
    home.configFile."TabNine/TabNine.toml".source =
      "${configDir}/tabnine/TabNine.toml";
  };
}
