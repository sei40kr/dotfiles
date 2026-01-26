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
      # Hardware / System Info
      usbutils
      pciutils
      lshw
      dmidecode
      lm_sensors
      # Monitoring / Process
      htop
      btop
      psmisc
      lsof
      iotop
      sysstat
      # Network
      curl
      wget
      dig
      mtr
      tcpdump
      ethtool
      socat
      # File / Text Utils
      ripgrep
      fd
      tree
      file
      unzip
      zip
      jq
      which
      # Disk
      ncdu
      parted
      # Editors / Version Control
      vim
      git
    ];
  };
}
