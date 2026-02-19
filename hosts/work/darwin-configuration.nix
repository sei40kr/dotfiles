{ inputs, pkgs, ... }:

{
  imports = [ inputs.self.darwinModules.host-shared ];

  nixpkgs.hostPlatform = "aarch64-darwin";

  networking.hostName = "work";
  time.timeZone = "Asia/Tokyo";

  programs.zsh.enable = true;
  security.pam.services.sudo_local.touchIdAuth = true;

  nix.gc = {
    automatic = true;
    interval = {
      Weekday = 0;
      Hour = 3;
      Minute = 0;
    };
  };

  users.users.sei40kr = {
    home = "/Users/sei40kr";
  };

  environment.systemPackages = with pkgs; [
    code-cursor
    notion-app
    slack
  ];

  homebrew = {
    enable = true;
    onActivation.cleanup = "zap";
    casks = [
      "docker"
      "figma"
    ];
  };

  system.stateVersion = 6;
}
