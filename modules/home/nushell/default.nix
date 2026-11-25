{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  shellCfg = config.modules.shell;
  cfg = shellCfg.nushell;

  atuin_nu = pkgs.runCommandLocal "atuin.nu" { buildInputs = [ pkgs.atuin ]; } ''
    export HOME=$(mktemp -d)
    mkdir -p $HOME/.local/share/atuin

    atuin init nu >$out

    # config.nu replaces this hook, so fail loudly if atuin renames it.
    grep -q '^let _atuin_pre_prompt = ' $out
  '';

  zoxide_nu = pkgs.runCommandLocal "zoxide.nu" { buildInputs = [ pkgs.zoxide ]; } ''
    zoxide init nushell >$out
  '';
in
{
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
        # Extended key codes tell ctrl+m, ctrl+i and ctrl+[ apart from Enter, Tab and Esc,
        # which share their legacy bytes, so they no longer act as those keys on their own.
        $env.config.keybindings ++= [
          {
            name: ctrl_m_enter
            modifier: control
            keycode: char_m
            mode: [emacs vi_insert vi_normal]
            event: { send: enter }
          }
          {
            name: ctrl_i_tab
            modifier: control
            keycode: char_i
            mode: [emacs vi_insert vi_normal]
            # Nushell's default Tab binding
            event: {
              until: [
                { send: menu name: completion_menu }
                { send: menunext }
                { edit: complete }
              ]
            }
          }
          {
            name: ctrl_lbracket_esc
            modifier: control
            keycode: "char_["
            mode: [emacs vi_insert vi_normal]
            event: { send: esc }
          }
        ]

        source ${pkgs.nu_scripts}/share/nu_scripts/modules/data_extraction/ultimate_extractor.nu
        alias x = extract

        source ${atuin_nu}
        # atuin ends each history entry from a `job spawn` thread. `job list` reports that
        # thread until it finishes, so Starship painted a background-job indicator after
        # every command. Detach the bookkeeping through sh instead, as atuin's zsh
        # integration does.
        $env.config = ($env.config? | default {} | upsert hooks.pre_prompt {
          default []
          | where {|hook| $hook != $_atuin_pre_prompt }
          | append {||
            if "ATUIN_HISTORY_ID" in $env {
              ^sh -c 'ATUIN_LOG=error atuin history end --exit="$1" -- "$2" >/dev/null 2>&1 &' sh $env.LAST_EXIT_CODE $env.ATUIN_HISTORY_ID
              hide-env ATUIN_HISTORY_ID
            }
          }
        })
        source ${zoxide_nu}
      '';
    };
    programs.carapace.enable = true;

    # Keep Zsh as the login shell so that its startup files set up the Nix and home-manager
    # environment, then hand interactive login sessions over to Nushell before the rest of
    # ~/.zshrc runs. `zsh -lic <command>` runs its command instead.
    programs.zsh.initContent = lib.mkOrder 0 ''
      [[ -o login && -z $NU_VERSION && -z $ZSH_EXECUTION_STRING ]] && exec ${lib.getExe config.programs.nushell.package}
    '';

    modules.shell.enable = true;
    modules.shell.atuin.enable = true;
    # The atuin integration is sourced above; home-manager's copy would register the history
    # hooks a second time, recording every command twice.
    programs.atuin.enableNushellIntegration = false;
    modules.shell.starship.enable = true;
    modules.shell.zsh.enable = true;
  };
}
