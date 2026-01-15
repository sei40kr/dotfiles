{ inputs, perSystem, ... }:
{
  # Minimal test host configuration

  # Phase 1: Docker module test
  imports = [
    inputs.self.nixosModules.docker
  ];

  modules.services.docker.enable = true;
  modules.services.docker.compose.enable = true;

  # Basic system settings
  networking.hostName = "test";
  time.timeZone = "Asia/Tokyo";

  nixpkgs.hostPlatform = "x86_64-linux";
  nixpkgs.config.allowUnfree = true;

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
