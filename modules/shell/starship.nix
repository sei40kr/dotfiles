{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.shell.starship;

  starship-nerd-font-symbols-preset =
    pkgs.runCommandLocal "starship-nerd-font-symbols-preset-${pkgs.starship.version}" { }
      ''
        mkdir -p $out/etc
        ${pkgs.starship}/bin/starship preset nerd-font-symbols >$out/etc/starship.toml
      '';

  starship_zsh = pkgs.runCommand "starship-zhook" { buildInputs = [ pkgs.starship ]; } ''
    XDG_CACHE_HOME=$(mktemp -d) ${pkgs.starship}/bin/starship init zsh >$out
  '';
  starship_nu = pkgs.runCommandLocal "starship.nu" { buildInputs = [ pkgs.starship ]; } ''
    starship init nu >$out
  '';
in
{
  options.modules.shell.starship = {
    enable = mkEnableOption "Starship";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ starship ];

    home.configFile."starship.toml".text = ''
      ${builtins.readFile "${starship-nerd-font-symbols-preset}/etc/starship.toml"}

      [shell]
      disabled = false
    '';

    programs.zsh.promptInit = ''
      . ${starship_zsh}
    '';

    modules.shell.nushell.rcInit = ''
      source ${starship_nu}
    '';
  };
}
