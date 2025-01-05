{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (builtins) readFile;
  inherit (lib)
    concatStringsSep
    mapAttrsToList
    mkEnableOption
    mkIf
    mkOption
    types
    ;
  shellCfg = config.modules.shell;
  cfg = shellCfg.nushell;

  atuin_nu = pkgs.runCommandLocal "atuin.nu" { buildInputs = [ pkgs.atuin ]; } ''
    # HACK: Atuin tries to create a directory in `homeless-shelter/.local/share`
    #  and fails because it doesn't have permission to create a directory in.
    #  We can work around this by setting `$HOME` to a temporary directory.
    export HOME=$(mktemp -d)
    mkdir -p $HOME/.local/share/atuin

    atuin init nu >$out
  '';

  zoxide_nu = pkgs.runCommandLocal "zoxide.nu" { buildInputs = [ pkgs.zoxide ]; } ''
    zoxide init nushell >$out
  '';

  config_nu = pkgs.substituteAll {
    inherit atuin_nu zoxide_nu;
    src = ../../config/nushell/config.nu;
    nu_scripts = "${pkgs.nu_scripts}/share/nu_scripts";
  };
in
{
  options.modules.shell.nushell = {
    enable = mkEnableOption "Nushell";

    rcInit = mkOption {
      type = types.lines;
      default = "";
    };
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ atuin ];

    home-manager.users.${config.user.name}.programs.nushell = {
      enable = true;
      extraConfig = ''
        ${readFile config_nu}

        ${concatStringsSep "\n" (mapAttrsToList (name: value: "alias ${name} = ${value}") shellCfg.aliases)}

        ${cfg.rcInit}
      '';
    };

    modules.shell.enable = true;
  };
}
