{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    mkIf
    types
    mkOption
    mkEnableOption
    ;
  inherit (types) nullOr str;
  cfg = config.modules.shell;
in
{
  options.modules.shell = {
    enable = mkEnableOption "Common shell configurations and tools";

    bat = {
      theme = mkOption {
        type = nullOr str;
        default = null;
      };
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      csvlens
      direnv
      dust
      fd
      eza
      hexyl
      htop
      jaq
      lurk
      netscanner
      pik
      prettyping
      procs
      rm-improved
      rustscan
      sd
      tealdeer
      trippy
      xcp
    ];

    services.lorri.enable = true;

    home.shellAliases = {
      u = "cd ..";
      cat = "bat";
      du = "dust";
      la = "eza -lbhHigUmuSa --time-style=long-iso --git";
      ll = "eza -lbF --git";
      ls = "eza";
      tree = "eza -T";
      top = "htop";
      ping = "prettyping --nolegend";
      ps = "procs";
    };

    programs.bat = {
      enable = true;
      config = mkIf (cfg.bat.theme != null) { inherit (cfg.bat) theme; };
    };

    home.sessionVariables = {
      MANPAGER = "sh -c 'col -bx | bat -l man -p'";
      MANROFFOPT = "-c";
    };
  };
}
