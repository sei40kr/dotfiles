{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.shell.zsh.zinit;
  # Ice Modifiers
  # https://github.com/zdharma/zinit#ice-modifiers
  iceOpts = with types; {
    # Conditional Loading
    wait = mkOpt (nullOr (either int str)) null;
    "if" = mkOpt (nullOr str) null;
    has = mkOpt (nullOr str) null;
    trigger-load = mkOpt (listOf str) [ ];

    # Selection of Files
    pick = mkOpt (nullOr str) null;
    src = mkOpt (nullOr str) null;
    multisrc = mkOpt (nullOr str) null;

    # Plugin Output
    lucid = mkBoolOpt false;

    # Completions
    blockf = mkBoolOpt false;

    # Command Execution After Cloning, Updating, or Loading
    atclone = mkOpt lines "";
    atpull = mkOpt lines "";
    atinit = mkOpt lines "";

    # Others
    as = mkOpt (nullOr (enum [ "program" "command" "completion" "null" ])) null;
    id-as = mkOpt (nullOr str) null;
    compile = mkOpt (nullOr str) null;
  };
  pluginIceType = with types;
    submodule {
      options = iceOpts // {
        # Others
        bindmap = mkOpt attrs { };
        trackbinds = mkBoolOpt false;
      };
    };
  snippetIceType = with types;
    submodule {
      options = iceOpts // {
        # Cloning Options
        svn = mkBoolOpt false;
      };
    };
  pluginType = with types;
    submodule {
      options = {
        source = mkOpt (oneOf [ str path ]) null;
        ice = mkOpt pluginIceType { };
        config = mkOpt lines "";
      };
    };
  snippetType = with types;
    submodule {
      options = {
        source = mkOpt (either str path) null;
        ice = mkOpt snippetIceType { };
        config = mkOpt lines "";
      };
    };
  generateIceArgs = { svn ? false, wait, has, trigger-load, pick, src, multisrc
    , lucid, blockf, atclone, atpull, atinit, as, id-as, compile
    , trackbinds ? false, bindmap ? { }, ... }@ice:
    concatStringsSep " " (filter (v: v != null) [
      (if svn then "svn" else null)
      (if wait != null then "wait${escapeShellArg wait}" else null)
      (if ice."if" != null then "if${escapeShellArg ice."if"}" else null)
      (if has != null then "has${escapeShellArg has}" else null)
      (if trigger-load != [ ] then
        "trigger-load${escapeShellArg (concatStringsSep ";" trigger-load)}"
      else
        null)
      (if pick != null then "pick${escapeShellArg pick}" else null)
      (if src != null then "src${escapeShellArg src}" else null)
      (if multisrc != null then "multisrc${escapeShellArg multisrc}" else null)
      (if lucid then "lucid" else null)
      (if blockf then "blockf" else null)
      (if atclone != "" then "atclone${escapeShellArg atclone}" else null)
      (if atpull != "" then "atpull${escapeShellArg atpull}" else null)
      (if atinit != "" then "atinit${escapeShellArg atinit}" else null)
      (if as != null then "as${escapeShellArg as}" else null)
      (if id-as != null then "id-as${escapeShellArg id-as}" else null)
      (if compile != null then "compile${escapeShellArg compile}" else null)
      (if trackbinds then "trackbinds" else null)
      (if bindmap != { } then
        "bindmap${
          escapeShellArg
          (concatStringsSep ";" (mapAttrsToList (k: v: "${k} -> ${v}") bindmap))
        }"
      else
        null)
    ]);
in {
  options.modules.shell.zsh.zinit = with types; {
    plugins = mkOpt (listOf pluginType) [ ];
    snippets = mkOpt (listOf snippetType) [ ];
  };

  config.home-manager.users.${config.user.name}.programs.zsh.initExtraBeforeCompInit =
    optionalString (cfg.plugins != [ ] || cfg.snippets != [ ]) ''
      # zinit deps
      PATH="''${PATH:+''${PATH}:}${pkgs.curl}/bin:${pkgs.git}/bin:${pkgs.subversion}/bin"

      declare -A ZINIT
      ZINIT[BIN_DIR]=${pkgs.zinit}/share/zinit
      . "''${ZINIT[BIN_DIR]}/zinit.zsh"

      ${concatMapStringsSep "\n" ({ source, ice, config }:
        let iceArgs = generateIceArgs ice;
        in ''
          ${config}${
            optionalString (iceArgs != "") ''
              zinit ice ${iceArgs}
            ''
          }zinit light ${source}
        '') cfg.plugins}

      ${concatMapStringsSep "\n" ({ source, ice, config }:
        let iceArgs = generateIceArgs ice;
        in ''
          ${config}${
            optionalString (iceArgs != "") ''
              zinit ice ${iceArgs}
            ''
          }zinit snippet ${source}
        '') cfg.snippets}
    '';
}
