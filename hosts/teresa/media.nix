{ config, lib, pkgs, ... }:

{
  # setup an user for all media services
  users = {
    groups.media = { };
    users.media = {
      group = "media";
      isSystemUser = true;
    };
  };

  systemd.tmpfiles.rules = [
    # directory where the movies/tv shows/books should go to
    "d /var/lib/media/books 0755 media media"
    "d /var/lib/media/books/books 0755 media media"
    "d /var/lib/media/books/audiobooks 0755 media media"
    "d /var/lib/media/movies 0755 media media"
    "d /var/lib/media/shows 0755 media media"

    # directory where the torrents should go
    "d /var/lib/torrents/incomplete 0755 media media"
    "d /var/lib/torrents/complete 0755 media media"
  ];

  services = {
    caddy.enable = lib.mkDefault true;

    transmission = {
      enable = true;
      user = "media";
      group = "media";
      settings = {
        download-queue-size = 5;
        download-dir = "/var/lib/torrents/complete";
        incomplete-dir = "/var/lib/torrents/incomplete";
        incomplete-dir-enabled = true;
      };
    };

    caddy.virtualHosts."jellyfin.glorifiedgluer.com" = {
      extraConfig = ''
        reverse_proxy 127.0.0.1:8096
      '';
    };

    jellyfin = {
      user = "media";
      group = "media";
      enable = true;
      openFirewall = true;
    };

    radarr = {
      enable = true;
      user = "media";
      group = "media";
      openFirewall = true;
    };

    sonarr = {
      enable = true;
      user = "media";
      group = "media";
      openFirewall = true;
    };

    bazarr = {
      enable = true;
      user = "media";
      group = "media";
      openFirewall = true;
    };

    prowlarr = {
      enable = true;
      openFirewall = true;
    };
  };
}
