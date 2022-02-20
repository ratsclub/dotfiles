{ config, pkgs, username, ... }:

let
  inherit (config.meta) username;
in
{
  programs.firefox = {
    enable = true;
    profiles."${config.meta.username}" = {
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
      bitwarden
      multi-account-containers
      privacy-badger
      ublock-origin
    ];
  };
}
