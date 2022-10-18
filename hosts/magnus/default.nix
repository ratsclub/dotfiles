{ config, inputs, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
      ../../modules/common/nix.nix

      inputs.homeManager.nixosModules.home-manager
      inputs.hardware.nixosModules.lenovo-thinkpad-t495
      inputs.agenix.nixosModules.age
    ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  networking.hostName = "magnus";
  networking.networkmanager.enable = true;

  time.timeZone = "America/Sao_Paulo";

  i18n.defaultLocale = "en_US.utf8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "pt_BR.utf8";
    LC_IDENTIFICATION = "pt_BR.utf8";
    LC_MEASUREMENT = "pt_BR.utf8";
    LC_MONETARY = "pt_BR.utf8";
    LC_NAME = "pt_BR.utf8";
    LC_NUMERIC = "pt_BR.utf8";
    LC_PAPER = "pt_BR.utf8";
    LC_TELEPHONE = "pt_BR.utf8";
    LC_TIME = "pt_BR.utf8";
  };

  services.xserver.enable = true;
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;
  services.xserver = {
    layout = "br";
    xkbVariant = "nodeadkeys";
  };

  console.keyMap = "br-abnt2";

  services.printing.enable = true;

  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  users.users.victor = {
    isNormalUser = true;
    description = "victor";
    extraGroups = [ "networkmanager" "wheel" ];
  };

  nixpkgs.config.allowUnfree = true;

  age = {
    secrets.mailbox = {
      file = ../../secrets/mailbox.age;
      owner = "victor";
    };
    identityPaths = [ "/home/victor/.ssh/id_ed25519" ];
  };
  home-manager = {
    useUserPackages = true;
    users.victor = {
      imports = [
        ../../home/modules
        ../../home/modules/bash.nix
        ../../home/modules/cli.nix
        ../../home/modules/doom
        ../../home/modules/git.nix
        ../../home/modules/email.nix

        # gui
        ../../home/modules/chromium.nix
        ../../home/modules/firefox.nix
        ../../home/modules/gui.nix
        ../../home/modules/vscodium.nix
      ];
    };
    extraSpecialArgs = { inherit inputs pkgs; };
  };

  system.stateVersion = "22.05";
}
