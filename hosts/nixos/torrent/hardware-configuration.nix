{ config, lib, ... }:

{
  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usbhid" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  hardware = {
    cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
    opengl.enable = true;
    # high-resolution display
    video.hidpi.enable = lib.mkDefault true;
  };

  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.legacy_470.overrideAttrs (_attrs: {
    postPatch = ''
      rm systemd/system-sleep/nvidia
    '';
  });
  systemd.services = {
    "nvidia-suspend".enable = false;
    "nvidia-hibernate".enable = false;
    "nvidia-resume".enable = false;
  };

  fileSystems = {
    "/" =
      {
        device = "/dev/disk/by-uuid/560bee25-b233-4322-8c46-1bfc9db75ec5";
        fsType = "ext4";
      };

    "/boot" =
      {
        device = "/dev/disk/by-uuid/8FCA-7C43";
        fsType = "vfat";
      };
  };

  swapDevices = [{ device = "/dev/disk/by-uuid/2367e722-f68f-47de-b90d-a3721928b09f"; }];

  time.hardwareClockInLocalTime = true;

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  services.xserver.videoDrivers = [ "nvidia" ];
}
