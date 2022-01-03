{ config, lib, hardware, home-manager, nixpkgs, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
      ../../modules/device.nix

      ../../lib/user.nix

      hardware.lenovo-thinkpad-t495
    ];

  device.type = "graphical";

  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    # source: https://grahamc.com/blog/erase-your-darlings
    initrd.postDeviceCommands = lib.mkAfter ''
      zfs rollback -r rpool/local/root@blank
    '';

    cleanTmpDir = true;
    consoleLogLevel = 7;
    supportedFilesystems = [ "ntfs" ];

    # source: https://grahamc.com/blog/nixos-on-zfs
    kernelParams = [ "elevator=none" ];
  };

  # Set your time zone.
  time.timeZone = "America/Sao_Paulo";

  networking = {
    hostId = "76cb839b";
    hostName = "t495";
    networkmanager.enable = true;
  };

  programs.noisetorch.enable = true;

  services.zfs = {
    autoScrub.enable = true;
    autoSnapshot.enable = true;
  };

  # Enable sound.
  sound.enable = true;
  hardware = {
    pulseaudio.enable = false;
    ledger.enable = true;
  };

  services = {
    xserver = {
      enable = true;
      videoDrivers = [ "amdgpu" ];
      libinput.enable = true;

      desktopManager = {
        gnome.enable = true;
        xterm.enable = false;
      };

      displayManager.gdm = {
        enable = true;
        wayland = true;
      };
    };

    pipewire = {
      enable = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };
      pulse.enable = true;
    };
  };

  virtualisation = {
    docker.enable = true;
    libvirtd.enable = true;
  };

  environment = {
    etc = {
      "nix/channels/nixpkgs".source = nixpkgs;
      "nix/channels/home-manager".source = home-manager;
      "NetworkManager/system-connections" = {
        source = "/persist/etc/NetworkManager/system-connections/";
      };

    };

    systemPackages = with pkgs; [
      gnome.gnome-boxes
    ];
  };

  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';

    nixPath = [
      "nixpkgs=/etc/nix/channels/nixpkgs"
      "home-manager=/etc/nix/channels/home-manager"
    ];

    gc = {
      automatic = true;
      options = "--delete-older-than 2d";
    };

    registry.nixpkgs.flake = nixpkgs;
    autoOptimiseStore = true;
  };

  systemd.tmpfiles.rules = [
    "L /var/lib/bluetooth - - - - /persist/var/lib/bluetooth"
  ];

  system.stateVersion = "21.11";
}

