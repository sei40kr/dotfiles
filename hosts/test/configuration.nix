{ inputs, ... }:
{
  # Minimal test host configuration

  imports = [ inputs.self.nixosModules.host-shared ];

  modules.services.docker.enable = true;
  modules.services.docker.compose.enable = true;
  modules.services.ssh.enable = true;
  modules.services.wireguard.enable = true;
  modules.i18n.japanese.enable = true;
  modules.desktop.wm.niri.enable = true;
  modules.desktop.regreet.enable = true;
  modules.desktop.apps.thunar.enable = true;
  modules.desktop.apps.steam.enable = true;
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
