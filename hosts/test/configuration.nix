{ inputs, perSystem, ... }:
{
  # Minimal test host configuration

  # Phase 1: Docker module test
  # Phase 2: SSH and WireGuard modules test
  # Phase 3: Desktop modules test
  # Phase 7d-4: App modules test (nixos)
  imports = [
    inputs.self.nixosModules.host-shared
    inputs.self.nixosModules.docker
    inputs.self.nixosModules.ssh
    inputs.self.nixosModules.wireguard
    inputs.self.nixosModules.japanese
    inputs.self.nixosModules.niri
    inputs.self.nixosModules.regreet
    inputs.self.nixosModules.thunar
    inputs.self.nixosModules.steam
    inputs.self.nixosModules.dunst
    inputs.self.nixosModules.chrome
    inputs.self.nixosModules.theme-shared
    inputs.self.nixosModules.graphite-theme
    inputs.self.nixosModules.whitesur-theme
    inputs.self.nixosModules.orchis-theme
  ];

  modules.services.docker.enable = true;
  modules.services.docker.compose.enable = true;
  modules.services.ssh.enable = true;
  modules.services.wireguard.enable = true;
  modules.i18n.japanese.enable = true;
  modules.desktop.wm.niri.enable = true;
  modules.desktop.regreet.enable = true;
  modules.desktop.apps.thunar.enable = true;
  modules.desktop.apps.steam.enable = true;
  modules.desktop.apps.dunst.enable = true;
  modules.desktop.browsers.chrome.enable = true;
  modules.desktop.theme.active = "graphite";

  # Basic system settings
  networking.hostName = "test";
  time.timeZone = "Asia/Tokyo";

  nixpkgs.hostPlatform = "x86_64-linux";

  # Bootloader (minimal test configuration)
  boot.loader.grub.enable = false;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Dummy filesystem configuration (for build testing)
  fileSystems."/" = {
    device = "/dev/null";
    fsType = "ext4";
  };

  # User definition
  users.users.sei40kr = {
    isNormalUser = true;
    description = "Test User";
    extraGroups = [ "wheel" ];
  };

  system.stateVersion = "23.11";
}
