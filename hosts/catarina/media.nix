{ config, ... }:

let
  mediaPath = config.nixflix.mediaDir;
  olgaLocalIp = "192.168.18.5";
in
{
  fileSystems."${mediaPath}" = {
    device = "${olgaLocalIp}:/volume1/media";
    fsType = "nfs";
    options = [
      "nfsvers=4.1"
      "x-systemd.automount"
      "_netdev"
      "nofail"
      "noatime"
    ];
  };

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
    downloadsDir = "${mediaPath}/downloads";
    mediaUsers = [ "victor" ];
    postgres.enable = true;

    # Media lives on an NFS mount reached over Tailscale, so the arr services
    # must not touch their media dirs until both are up.
    serviceDependencies = [
      "tailscaled.service"
      "data-media.mount"
    ];

    sonarr = {
      enable = true;
      mediaDirs = [ "${mediaPath}/tvshows" ];
      group = "users";
      config = {
        apiKey._secret = config.age.secrets.sonarr-api-key.path;
        hostConfig.password._secret = config.age.secrets.sonarr-password.path;
      };
    };

    radarr = {
      enable = true;
      group = "users";
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
      group = "users";

      downloadsDir = "/var/lib/sabnzbd/work";

      settings = {
        misc = {
          api_key._secret = config.age.secrets.sabnzbd-api-key.path;
          nzb_key._secret = config.age.secrets.sabnzbd-nzb-key.path;
          complete_dir = "${mediaPath}/downloads/usenet/complete";
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
            preferred = 79;
            max = 80;
          }
          {
            name = "WEBRip-1080p";
            preferred = 79;
            max = 80;
          }
          {
            name = "Bluray-1080p";
            preferred = 79;
            max = 80;
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

  # Accessible for Bazarr that runs on another machine
  networking.firewall.allowedTCPPorts = [
    config.services.sonarr.settings.server.port
    config.services.radarr.settings.server.port
  ];

  # SABnzbd writes its downloads to the olga NFS mount, so don't start it until
  # the mount is up. The arr services get this via nixflix.serviceDependencies
  # above; the SABnzbd module doesn't consume that option, so wire it directly.
  # (List-valued unit deps merge with the module's own definition.)
  systemd.services.sabnzbd = {
    after = [ "data-media.mount" ];
    requires = [ "data-media.mount" ];
  };
}
