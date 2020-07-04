{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./emacs.nix ./idea.nix ./vscode.nix ];
}
