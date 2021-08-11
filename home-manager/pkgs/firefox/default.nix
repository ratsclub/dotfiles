{ pkgs, inputs, system, ... }:

let
  pkgs = import inputs.nixpkgs {
    inherit system;
    overlays = [
      inputs.nur.overlay
    ];
    config = {
      allowUnfree = true;
    };
  };
in
{
  programs.firefox = {
    enable = true;
    profiles."ratsclub" = {
      settings = {
        # https://wiki.archlinux.org/title/Firefox#Hardware_video_acceleration
        "gfx.webrender.all" = true;
        "browser.quitShortcut.disabled" = true;
        "media.ffmpeg.vaapi.enabled" = true;
        "media.ffvpx.enabled" = true;
        "media.navigator.mediadatadecoder_vpx_enabled" = true;
      };
    };
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      privacy-badger
      ublock-origin
      bitwarden
    ];
  };
}
