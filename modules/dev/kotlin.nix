{ config, lib, pkgs, ... }:

with lib; {
  options.modules.dev.kotlin.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.kotlin.enable {
    modules.dev = {
      editors = {
        doomEmacs.variables.lsp-clients-kotlin-server-executable =
          "${pkgs.my.kotlin-language-server}/bin/kotlin-language-server";
        tools.packages = with pkgs;
          with pkgs.my; [
            kotlin-language-server
            ktlint
          ];
      };
      tools = {
        jenv.enable = mkForce true;
        maven.enable = mkForce true;
        gradle.enable = mkForce true;
        springBoot.enable = mkForce true;
      };
    };

    user.packages = with pkgs; [ kotlin ];
  };
}
