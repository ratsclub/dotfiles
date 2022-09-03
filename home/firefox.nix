{ config, pkgs, username, ... }:

let
  inherit (config.home) username;
in
{
  programs.firefox = {
    enable = true;
    profiles."${username}" = {
      isDefault = true;
      settings = {
        # https://wiki.archlinux.org/title/Firefox#Hardware_video_acceleration
        "gfx.webrender.all" = true;
        "browser.quitShortcut.disabled" = true;
        "media.ffmpeg.vaapi.enabled" = true;
        "media.ffvpx.enabled" = true;
        "media.navigator.mediadatadecoder_vpx_enabled" = true;

        # Disable what's new toolbar
        "browser.messaging-system.whatsNewPanel.enabled" = false;
        # No Pocket
        "extensions.pocket.enabled" = false;
        # No Firefox Sync
        "identity.fxaccounts.enabled" = false;
        # No recommended extensions
        "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons" = false;
        # No recommended features
        "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features" = false;
        # Don't show bookmarks toolbar
        "browser.toolbars.bookmarks.visibility" = "never";
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
