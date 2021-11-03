{ config, hardware, home-manager, inputs, nixpkgs, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
      ../../modules/device.nix

      hardware.lenovo-thinkpad-t495
    ];

  device.type = "graphical";

  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
    kernelPackages = pkgs.linuxPackages_latest;
    kernelParams = [
      "quiet"
      "splash"
      "udev.log_priority=3"
    ];
    supportedFilesystems = [ "ntfs" ];
    plymouth.enable = true;

    cleanTmpDir = true;
  };

  # Set your time zone.
  time.timeZone = "America/Sao_Paulo";

  networking = {
    hostName = "t495";

    useDHCP = false;
    interfaces.enp3s0f0.useDHCP = true;
    interfaces.enp4s0.useDHCP = true;
    interfaces.wlp1s0.useDHCP = true;
  };

  # Enable sound.
  sound.enable = true;
  hardware = {
    pulseaudio.enable = true;
    ledger.enable = true;
  };

  programs = {
    kdeconnect = {
      enable = true;
      package = pkgs.gnomeExtensions.gsconnect;
    };
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
  };

  users.users.victor = {
    isNormalUser = true;
    extraGroups = [ 
      "docker" 
      "kvm"
      "libvirtd" 
      "networkmanager" 
      "wheel" 
    ];
    password = "changeme";
  };

  virtualisation = {
    docker.enable = true;
    libvirtd.enable = true;
  };

  environment = {
    etc = {
      "nix/channels/nixpkgs".source = nixpkgs;
      "nix/channels/home-manager".source = home-manager;
    };
  };

  environment.systemPackages = with pkgs; [
    gnome.gnome-boxes
  ];

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

  system.stateVersion = "21.05";
}

