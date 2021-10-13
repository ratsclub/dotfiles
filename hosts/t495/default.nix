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

    sway = {
      enable = true;
      wrapperFeatures.gtk = true; # so that gtk works properly
      extraPackages = with pkgs; [
        dmenu
        swaylock
        swayidle
        xwayland
        mako
        grim
        slurp
        wl-clipboard
        wf-recorder
        (python38.withPackages (ps: with ps; [ i3pystatus keyring ]))
      ];
      extraSessionCommands = ''
        export SDL_VIDEODRIVER=wayland
        export QT_QPA_PLATFORM=wayland
        export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
        export _JAVA_AWT_WM_NONREPARENTING=1
        export MOZ_ENABLE_WAYLAND=1
      '';
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
    extraGroups = [ "wheel" "docker" "networkmanager" "libvirtd" ];
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

