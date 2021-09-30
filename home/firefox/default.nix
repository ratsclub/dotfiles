{ super, pkgs, ... }:

{
  programs.firefox = {
    enable = super.device.type == "graphical";
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      bitwarden
      https-everywhere
      multi-account-containers
      privacy-badger
      ublock-origin
    ];
  };
}
