{ config, lib, pkgs, ... }:

with lib; {
  options.modules.dev.editors.idea.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.editors.idea.enable {
    modules.dev.editors = {
      fonts.enable = mkForce true;
      ideavim.enable = mkForce true;
      tabnine.enable = mkForce true;
    };

    my.packages = with pkgs; [ jetbrains.idea-ultimate ];
  };
}
