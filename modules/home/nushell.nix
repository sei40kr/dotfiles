{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  inherit (builtins) match;
  inherit (lib)
    concatStringsSep
    mapAttrsToList
    mkEnableOption
    mkIf
    ;
  shellCfg = config.modules.shell;
  cfg = shellCfg.nushell;

  atuin_nu = pkgs.runCommandLocal "atuin.nu" { buildInputs = [ pkgs.atuin ]; } ''
    export HOME=$(mktemp -d)
    mkdir -p $HOME/.local/share/atuin

    atuin init nu >$out
  '';

  zoxide_nu = pkgs.runCommandLocal "zoxide.nu" { buildInputs = [ pkgs.zoxide ]; } ''
    zoxide init nushell >$out
  '';
in
{
  imports = [
    inputs.self.homeModules.atuin
    inputs.self.homeModules.shell-shared
    inputs.self.homeModules.starship
  ];

  options.modules.shell.nushell = {
    enable = mkEnableOption "Nushell";
  };

  config = mkIf cfg.enable {
    programs.nushell = {
      enable = true;
      extraConfig = ''
        $env.config = {
          show_banner: false
        }

        source ${pkgs.nu_scripts}/share/nu_scripts/modules/data_extraction/ultimate_extractor.nu
        alias x = extract

        source ${pkgs.nu_scripts}/share/nu_scripts/nu-hooks/nu-hooks/direnv/direnv.nu

        source ${atuin_nu}
        source ${zoxide_nu}

        ${concatStringsSep "\n" (
          mapAttrsToList (
            name: value:
            if match ".*(;|\\|).*" value != null then
              "def ${name} [] { ${value} }"
            else
              "alias ${name} = ${value}"
          ) shellCfg.aliases
        )}
      '';
    };
    programs.carapace.enable = true;

    modules.shell.enable = true;
    modules.shell.atuin.enable = true;
    modules.shell.starship.enable = true;
  };
}
