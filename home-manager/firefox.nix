{ pkgs, ... }:

{
  programs = {
    firefox = {
      enable = true;
      profiles.ratsclub = {
        settings = {
          # https://wiki.archlinux.org/title/Firefox#Hardware_video_acceleration
          "gfx.webrender.all" = true;
          "browser.quitShortcut.disabled" = true;
          "media.ffmpeg.vaapi.enabled" = true;
          "media.ffvpx.enabled" = true;
          "media.navigator.mediadatadecoder_vpx_enabled" = true;
        };
      };
    };
  };
}
