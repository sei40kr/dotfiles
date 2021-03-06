{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.tools.ripgrep;
in {
  options.modules.shell.tools.ripgrep.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ ripgrep ];
    home.file.".ripgreprc".source = "${configDir}/ripgrep/ripgreprc";
    modules.shell.aliases = { notes = "rg 'TODO|FIXME|HACK|OPTIMIZE|REVIEW'"; };
  };
}
