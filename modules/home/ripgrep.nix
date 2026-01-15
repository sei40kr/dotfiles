{
  config,
  lib,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;

  cfg = config.modules.shell.ripgrep;
in
{
  options.modules.shell.ripgrep = {
    enable = mkEnableOption "ripgrep";
  };

  config = mkIf cfg.enable {
    programs.ripgrep = {
      enable = true;
      arguments = [
        "--smart-case"
        "--follow"
        "--max-columns=80"
      ];
    };

    home.shellAliases = {
      notes = "rg 'TODO|FIXME|HACK|OPTIMIZE|REVIEW'";
    };
  };
}
