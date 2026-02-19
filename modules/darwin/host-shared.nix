{ pkgs, ... }:

{
  config = {
    nix = {
      package = pkgs.lix;
      extraOptions = "experimental-features = nix-command flakes";
      settings = {
        trusted-users = [
          "root"
          "@admin"
        ];
        keep-outputs = true;
      };
    };

    nixpkgs.config.allowUnfree = true;

    environment.systemPackages = with pkgs; [
      # Monitoring / Process
      htop
      btop
      # Network
      curl
      wget
      # File / Text Utils
      ripgrep
      fd
      tree
      file
      unzip
      zip
      jq
      # Editors / Version Control
      vim
      git
    ];
  };
}
