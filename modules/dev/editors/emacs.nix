{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.editors.emacs.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.editors.emacs.enable {
    programs.emacs = {
      enable = true;
      package = pkgs.emacs.overrideAttrs (oldAttrs: { version = "27.0.91"; });
    };
  };
}
