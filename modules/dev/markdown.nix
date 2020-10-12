{ config, lib, pkgs, ... }:

with lib;
let githubCredential = import <secrets/config/grip-github-credential.nix>;
in {
  options.modules.dev.markdown.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.markdown.enable {
    modules.dev.editors.doomEmacs.variables = {
      grip-binary-path = "${pkgs.python37Packages.grip}/bin/grip";
      grip-github-password = githubCredential.password;
      grip-github-user = githubCredential.userName;
    };

    my.packages = with pkgs; [
      # TODO markdownlint-cli
      mdl
      python37Packages.grip
    ];
  };
}
