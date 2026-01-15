{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (builtins) fromTOML readFile;
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.shell.starship;

  starship-nerd-font-symbols-preset =
    pkgs.runCommandLocal "starship-nerd-font-symbols-preset-${pkgs.starship.version}" { }
      ''
        mkdir -p $out/etc
        XDG_CACHE_HOME=$(mktemp -d) ${pkgs.starship}/bin/starship preset nerd-font-symbols >$out/etc/starship.toml
      '';
in
{
  options.modules.shell.starship = {
    enable = mkEnableOption "Starship";
  };

  config = mkIf cfg.enable {
    programs.starship = {
      enable = true;
      settings = fromTOML (readFile "${starship-nerd-font-symbols-preset}/etc/starship.toml") // {
        shell.disabled = false;
      };
    };
  };
}
