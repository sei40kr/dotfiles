{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.shell.nushell;

  atuin_nu = pkgs.runCommandLocal "atuin.nu"
    {
      buildInputs = [ pkgs.atuin ];
    } ''
    atuin init nu >$out
  '';

  direnv_nu = pkgs.writeTextFile {
    name = "direnv.nu";
    text = ''
      $env.config.hooks.env_change.PWD = (
          $env.config.hooks.env_change.PWD | append ({ ||
              if (which direnv | is-empty) {
                  return
              }

              direnv export json | from json | default {} | load-env
          })
      )
    '';
  };

  starship_nu = pkgs.runCommandLocal "starship.nu"
    {
      buildInputs = [ pkgs.starship ];
    } ''
    starship init nu >$out
  '';
in
{
  options.modules.shell.nushell = with types; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ atuin ];

    home-manager.users.${config.user.name}.programs.nushell = {
      enable = true;
      extraConfig = ''
        $env.config = {
          show_banner: false
        }

        source ${pkgs.nu_scripts}/share/nu_scripts/modules/data_extraction/ultimate_extractor.nu

        source ${pkgs.nu_scripts}/share/nu_scripts/custom-completions/git/git-completions.nu

        source ${pkgs.nu_scripts}/share/nu_scripts/custom-completions/man/man-completions.nu

        source ${pkgs.nu_scripts}/share/nu_scripts/custom-completions/nix/nix-completions.nu

        source ${pkgs.nu_scripts}/share/nu_scripts/custom-completions/npm/npm-completions.nu

        source ${atuin_nu}

        source ${direnv_nu}

        source ${starship_nu}
      '';
    };
  };
}
