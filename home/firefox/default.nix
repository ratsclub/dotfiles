{ super, pkgs, username, ... }:

{
  programs.firefox = {
    enable = super.device.type == "graphical";
    profiles."${username}" = {
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
