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

    cleanTmpDir = true;
    consoleLogLevel = 7;
    supportedFilesystems = [ "ntfs" ];
  };

  # Set your time zone.
  time.timeZone = "America/Sao_Paulo";

  networking = {
    hostName = "t495";
    networkmanager.enable = true;
  };

  # Enable sound.
  sound.enable = true;
  hardware = {
    bluetooth.enable = true;
    ledger.enable = true;
    pulseaudio.enable = false;
  };

  programs = {
    dconf.enable = true;
    noisetorch.enable = true;
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

  system.stateVersion = "21.11";
}
