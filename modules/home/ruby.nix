{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf optionals;
  cfg = config.modules.dev.lang.ruby;
in
{
  options.modules.dev.lang.ruby = {
    enable = mkEnableOption "Ruby development environment";

    rails.enable = mkEnableOption "Ruby on Rails development tools";
  };

  config = mkIf cfg.enable {
    home.packages =
      with pkgs;
      (
        [
          ruby
          rubocop
        ]
        ++ optionals cfg.rails.enable [ rubyPackages.rails ]
      );

    programs.sheldon.settings.plugins = {
      omz-gem = {
        local = "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/gem";
        use = [ "*.plugin.zsh" ];
        apply = [ "defer" ];
      };
      omz-rake-fast = {
        local = "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/rake-fast";
        use = [ "*.plugin.zsh" ];
        apply = [ "defer" ];
      };
    };
  };
}
