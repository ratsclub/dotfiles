{ config, ... }:

let
  mediaPath = config.nixflix.mediaDir;
  olgaLocalIp = "192.168.18.5";
in
{
  fileSystems."${mediaPath}" = {
    device = "//${olgaLocalIp}/media";
    fsType = "cifs";
    options = [
      "credentials=${config.age.secrets.olga-smb-credentials.path}"
      "uid=0"
      "gid=${toString config.nixflix.globals.gids.media}"
      "file_mode=0664"
      "dir_mode=0775"
      "vers=3.1.1"
      "iocharset=utf8"
      "x-systemd.automount"
      "_netdev"
      "nofail"
      "noatime"
    ];
  };

  # olga SMB mount credentials (contains username=/password= for the media share)
  age.secrets.olga-smb-credentials.file = ../../secrets/catarina/media/olga/smb-credentials.age;

  # nixflix secrets
  age.secrets.sonarr-api-key.file = ../../secrets/catarina/media/sonarr/api-key.age;
  age.secrets.sonarr-password.file = ../../secrets/catarina/media/sonarr/password.age;
  age.secrets.radarr-api-key.file = ../../secrets/catarina/media/radarr/api-key.age;
  age.secrets.radarr-password.file = ../../secrets/catarina/media/radarr/password.age;
  age.secrets.prowlarr-api-key.file = ../../secrets/catarina/media/prowlarr/api-key.age;
  age.secrets.prowlarr-password.file = ../../secrets/catarina/media/prowlarr/password.age;
  age.secrets.seerr-api-key.file = ../../secrets/catarina/media/seerr/api-key.age;
  age.secrets.jellyfin-admin-password.file = ../../secrets/catarina/media/jellyfin/admin-password.age;

  # usenet secrets
  age.secrets.newsdemon-username.file = ../../secrets/catarina/media/newsdemon/username.age;
  age.secrets.newsdemon-password.file = ../../secrets/catarina/media/newsdemon/password.age;
  age.secrets.sabnzbd-api-key.file = ../../secrets/catarina/media/sabnzbd/api-key.age;
  age.secrets.sabnzbd-nzb-key.file = ../../secrets/catarina/media/sabnzbd/nzb-key.age;
  age.secrets.sabnzbd-web-password.file = ../../secrets/catarina/media/sabnzbd/web-password.age;
  age.secrets.nzbgeek-api-key.file = ../../secrets/catarina/media/nzbgeek/api-key.age;
  age.secrets.nzblife-api-key.file = ../../secrets/catarina/media/nzblife/api-key.age;

  nixflix = {
    enable = true;

    mediaDir = "/data/media";
    downloadsDir = "/data/downloads";
    mediaUsers = [ "victor" ];
    postgres.enable = true;

    # Media lives on an SMB mount reached over Tailscale, so the arr services
    # must not touch their media dirs until both are up.
    serviceDependencies = [
      "tailscaled.service"
      "data-media.mount"
    ];

    sonarr = {
      enable = true;
      mediaDirs = [ "${mediaPath}/tvshows" ];
      config = {
        apiKey._secret = config.age.secrets.sonarr-api-key.path;
        hostConfig.password._secret = config.age.secrets.sonarr-password.path;
      };
    };

    radarr = {
      enable = true;
      config = {
        apiKey._secret = config.age.secrets.radarr-api-key.path;
        hostConfig.password._secret = config.age.secrets.radarr-password.path;
      };
    };

    prowlarr = {
      enable = true;
      config = {
        apiKey._secret = config.age.secrets.prowlarr-api-key.path;
        hostConfig.password._secret = config.age.secrets.prowlarr-password.path;
        indexers = [
          {
            name = "NZBgeek";
            apiKey._secret = config.age.secrets.nzbgeek-api-key.path;
          }
          {
            name = "Nzb.life";
            apiKey._secret = config.age.secrets.nzblife-api-key.path;
          }
        ];
      };
    };

    usenetClients.sabnzbd = {
      enable = true;

      settings = {
        misc = {
          api_key._secret = config.age.secrets.sabnzbd-api-key.path;
          nzb_key._secret = config.age.secrets.sabnzbd-nzb-key.path;
          inet_exposure = "api+web (auth needed)";
          username = "admin";
          password._secret = config.age.secrets.sabnzbd-web-password.path;
        };
        servers = [
          {
            name = "NewsDemon";
            host = "news.newsdemon.com";
            port = 563;
            ssl = true;
            connections = 12;
            username._secret = config.age.secrets.newsdemon-username.path;
            password._secret = config.age.secrets.newsdemon-password.path;
          }
        ];
      };
    };

    torrentClients.qbittorrent = {
      enable = true;

      serverConfig = {
        # Seed to ratio 0, so the seed goal is met the instant a download
        # completes, then pause (action 0) rather than delete. Radarr/Sonarr
        # import first and remove the torrent afterwards via downloadarr's
        # removeCompletedDownloads.
        BitTorrent.Session = {
          GlobalMaxRatio = 0;
          MaxRatioAction = 0; # 0 = Pause
        };

        Preferences.WebUI = {
          Address = "*";
          LocalHostAuth = false;
          AuthSubnetWhitelistEnabled = true;
          # Tailscale subnet
          AuthSubnetWhitelist = "100.64.0.0/10";
        };
      };
    };

    downloadarr = {
      qbittorrent.removeCompletedDownloads = true;
      sabnzbd.removeCompletedDownloads = true;
    };

    # TRaSH-guide quality profiles + custom formats, re-synced daily.
    recyclarr = {
      enable = true;
      radarrQuality = "1080p";
      sonarrQuality = "1080p";
      cleanupUnmanagedProfiles.enable = true;
      config = {
        radarr.radarr.custom_formats = [
          {
            trash_ids = [ "dc98083864ea246d05a42df0d05f81cc" ]; # x265 (HD)
            assign_scores_to = [
              # [SQP] SQP-1 (1080p)
              {
                trash_id = "0896c29d74de619df168d23b98104b22";
                score = 200;
              }
            ];
          }
        ];

        radarr.radarr.quality_definition.qualities = [
          {
            name = "WEBDL-1080p";
            preferred = 84;
            max = 85;
          }
          {
            name = "WEBRip-1080p";
            preferred = 84;
            max = 85;
          }
          {
            name = "Bluray-1080p";
            preferred = 84;
            max = 85;
          }
        ];

        sonarr.sonarr.custom_formats = [
          {
            trash_ids = [ "47435ece6b99a0b477caf360e79ba0bb" ]; # x265 (HD)
            assign_scores_to = [
              # WEB-1080p (Alternative)
              {
                trash_id = "9d142234e45d6143785ac55f5a9e8dc9";
                score = 200;
              }
            ];
          }
        ];
      };
    };

    seerr = {
      enable = true;
      apiKey._secret = config.age.secrets.seerr-api-key.path;

      jellyfin = {
        hostname = "${olgaLocalIp}";
        port = 8096;
        useSsl = false;
        adminUsername = "victor";
        adminPassword._secret = config.age.secrets.jellyfin-admin-password.path;
        externalHostname = "https://flix.capivaras.dev";
      };
    };
  };

  networking.firewall.allowedTCPPorts = [
    config.services.sonarr.settings.server.port
    config.services.radarr.settings.server.port
    config.services.prowlarr.settings.server.port
    config.services.qbittorrent.webuiPort
  ];

  # Downloads live on local disk (/data/downloads), so file modes come from
  # qBittorrent's umask rather than the olga SMB mount's forced file_mode/dir_mode.
  # The arrs import torrents via move (copy then delete source), and deleting the
  # source needs group-write on its directory. qBittorrent, the arrs, and the media
  # group all share gid media, so a 0002 umask makes qBittorrent write 0664/0775
  # group-writable trees; without it the default 0022 umask yields 0644/0755 and the
  # arr's post-import delete fails, so the import never completes and the torrent is
  # never removed.
  systemd.services.qbittorrent.serviceConfig.UMask = "0002";
}
