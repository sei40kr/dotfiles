{ inputs, pkgs, ... }:

{
  imports = [
    inputs.self.nixosModules.agenix
    inputs.self.nixosModules.chrome
    inputs.self.nixosModules.de
    inputs.self.nixosModules.docker
    inputs.self.nixosModules.fontconfig
    inputs.self.nixosModules.graphite-theme
    inputs.self.nixosModules.niri
    inputs.self.nixosModules.orchis-theme
    inputs.self.nixosModules.regreet
    inputs.self.nixosModules.ssh
    inputs.self.nixosModules.steam
    inputs.self.nixosModules.theme-shared
    inputs.self.nixosModules.thunar
    inputs.self.nixosModules.whitesur-theme
    inputs.self.nixosModules.wireguard
    inputs.self.nixosModules.wm
    inputs.self.nixosModules.zsh
  ];

  config = {
    nix = {
      package = pkgs.lix;
      extraOptions = "experimental-features = nix-command flakes";
      settings = {
        trusted-users = [
          "root"
          "@wheel"
        ];
        keep-outputs = true;
      };
    };

    nixpkgs.config.allowUnfree = true;

    environment.systemPackages = with pkgs; [
      coreutils
      curl
      git
      vim
    ];
  };
}
