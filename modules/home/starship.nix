{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (builtins) fromTOML readFile;
  inherit (lib) mkEnableOption mkIf recursiveUpdate;
  cfg = config.modules.shell.starship;
  preset = fromTOML (readFile "${starship-nerd-font-symbols-preset}/etc/starship.toml");

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
      settings = recursiveUpdate preset {
        shell.disabled = false;
        # jj colocated repos keep git HEAD detached, so git_branch would always show "HEAD"
        format = "$username$hostname$localip$shlvl$singularity$kubernetes$directory$vcsh$fossil_branch$fossil_metrics$git_branch\${custom.jj}$all";
        git_branch.only_attached = true;
        custom.jj = {
          when = "jj root --ignore-working-copy";
          command = "jj log -r 'heads(::@ & bookmarks())' --no-graph --ignore-working-copy -T 'bookmarks.map(|b| b.name()).join(\" \")'";
          inherit (preset.git_branch) symbol;
          style = "bold purple";
          format = "on [$symbol$output]($style) ";
          require_repo = false;
        };
      };
    };
  };
}
