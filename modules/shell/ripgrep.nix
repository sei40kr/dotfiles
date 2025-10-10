{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;
  inherit (config.dotfiles) configDir;

  cfg = config.modules.shell.ripgrep;
in
{
  options.modules.shell.ripgrep = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ ripgrep ];

    home.file.".ripgreprc".source = "${configDir}/ripgrep/ripgreprc";

    modules.shell.aliases = {
      notes = "rg 'TODO|FIXME|HACK|OPTIMIZE|REVIEW'";
    };
  };
}
