{
  config,
  lib,
  ...
}:

let
  inherit (lib)
    concatStringsSep
    literalExpression
    mkDefault
    mkEnableOption
    mkIf
    mkOption
    optional
    optionalAttrs
    optionals
    optionalString
    types
    ;

  cfg = config.nixflix.tunarr;
  nixflix = config.nixflix;

  hostname = "${cfg.subdomain}.${nixflix.reverseProxy.domain}";

  # Caddy auto-enables HTTPS unless the site address is explicitly http://.
  caddyHostPrefix = optionalString (!nixflix.caddy.tls.enable) "http://";

  # Tunarr's own paths inside the image. `/config/tunarr` is the database
  # directory it derives from `getDefaultDatabaseDirectory()` whenever it
  # detects it is running in a container.
  containerDataDir = "/config/tunarr";
  containerPort = 8000;
in
{
  options.nixflix.tunarr = {
    enable = mkEnableOption "Tunarr live TV channel builder";

    image = mkOption {
      type = types.str;
      default = "docker.io/chrisbenincasa/tunarr:1.3.11@sha256:f4faefbbf0c3d6872e542135d766f590955a8dbba3019c93219b96cc6becd3dc";
      description = ''
        OCI image reference for Tunarr, pinned by digest.

        Tunarr has no NixOS package yet, so it runs from the upstream image.
        The digest is what makes the deployment reproducible; the tag is kept
        alongside it only so the version is readable at a glance.
      '';
    };

    dataDir = mkOption {
      type = types.path;
      default = "${nixflix.stateDir}/tunarr";
      defaultText = literalExpression ''"''${nixflix.stateDir}/tunarr"'';
      description = ''
        Host directory holding Tunarr's database, settings, logs and search
        index. Bind-mounted at `${containerDataDir}` inside the container.
      '';
    };

    mediaDirs = mkOption {
      type = types.listOf types.path;
      default = [ nixflix.mediaDir ];
      defaultText = literalExpression "[ nixflix.mediaDir ]";
      description = ''
        Library directories to expose to Tunarr, mounted read-only at the
        *same* paths inside the container.

        Keeping the paths identical means Jellyfin and Tunarr agree on what a
        file is called, so Tunarr can read media directly off disk without any
        path replacements configured.
      '';
    };

    port = mkOption {
      type = types.port;
      default = 8000;
      description = "Host port on which the Tunarr web UI and API are published.";
    };

    bindAddress = mkOption {
      type = types.str;
      default = if nixflix.reverseProxy.enable then "127.0.0.1" else "0.0.0.0";
      defaultText = literalExpression ''if nixflix.reverseProxy.enable then "127.0.0.1" else "0.0.0.0"'';
      description = "Host address the published port is bound to.";
    };

    openFirewall = mkOption {
      type = types.bool;
      default = false;
      description = "Open {option}`nixflix.tunarr.port` in the firewall.";
    };

    timeZone = mkOption {
      type = types.nullOr types.str;
      default = config.time.timeZone;
      defaultText = literalExpression "config.time.timeZone";
      description = ''
        Time zone passed to the container as `TZ`. Tunarr schedules programming
        and renders guide data in local time, so a container stuck on UTC
        produces a guide that is offset from every client.
      '';
    };

    logLevel = mkOption {
      type = types.enum [
        "trace"
        "debug"
        "info"
        "warn"
        "error"
        "fatal"
        "silent"
      ];
      default = "info";
      description = "Tunarr log level. Overrides whatever is set in the UI.";
    };

    proxyArtwork = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether Tunarr should proxy artwork through itself instead of
        redirecting clients straight at the media server.

        Only needed when whatever renders the guide cannot reach Jellyfin at
        the URL Tunarr hands out — not the case when both run on this host.
      '';
    };

    hdhr = {
      openDiscoveryPort = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Publish and open SSDP (1900/udp) so clients auto-discover Tunarr's
          HDHomeRun emulation.

          Leave this off when the tuner is added to Jellyfin by URL, and note
          that it conflicts with anything else claiming 1900/udp on the host
          (Jellyfin's own DLNA server, for one).
        '';
      };
    };

    gpu = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Pass GPU device nodes into the container for hardware transcoding.

          The image ships its own ffmpeg and VA-API drivers, so nothing has to
          be installed on the host beyond the kernel driver itself.
        '';
      };

      devices = mkOption {
        type = types.listOf types.str;
        default = [ "/dev/dri:/dev/dri" ];
        example = [ "/dev/dri/renderD128:/dev/dri/renderD128" ];
        description = "Device mappings attached to the container when {option}`nixflix.tunarr.gpu.enable` is set.";
      };
    };

    subdomain = mkOption {
      type = types.str;
      default = "tunarr";
      description = "Subdomain prefix for reverse proxy routing.";
    };

    reverseProxy = {
      expose = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to expose Tunarr via the nixflix reverse proxy.";
      };
    };

    environment = mkOption {
      type = types.attrsOf types.str;
      default = { };
      example = {
        TUNARR_SEARCH_MAX_MEMORY = "1024Mb";
      };
      description = "Extra environment variables for the Tunarr container.";
    };

    extraOptions = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = "Extra flags passed to the container runtime.";
    };
  };

  config = mkIf (nixflix.enable && cfg.enable) (
    let
      inherit (config.virtualisation.oci-containers.containers.tunarr) serviceName;
    in
    {
      assertions = [
        {
          assertion = cfg.timeZone != null;
          message = "nixflix.tunarr.timeZone must be set (or set time.timeZone); Tunarr builds its guide in local time.";
        }
      ];

      virtualisation.podman.enable = mkDefault true;
      virtualisation.oci-containers.backend = mkDefault "podman";

      virtualisation.oci-containers.containers.tunarr = {
        inherit (cfg) image extraOptions;

        ports = [
          "${cfg.bindAddress}:${toString cfg.port}:${toString containerPort}"
        ]
        ++ optional cfg.hdhr.openDiscoveryPort "1900:1900/udp";

        volumes = [
          "${cfg.dataDir}:${containerDataDir}"
        ]
        ++ map (dir: "${dir}:${dir}:ro") cfg.mediaDirs;

        devices = optionals cfg.gpu.enable cfg.gpu.devices;

        environment = {
          TUNARR_LOG_LEVEL = cfg.logLevel;
        }
        // optionalAttrs (cfg.timeZone != null) { TZ = cfg.timeZone; }
        // optionalAttrs cfg.proxyArtwork { TUNARR_PROXY_ARTWORK = "true"; }
        // optionalAttrs nixflix.reverseProxy.enable { TUNARR_SERVER_TRUST_PROXY = "true"; }
        // cfg.environment;
      };

      systemd.tmpfiles.settings."10-tunarr" = {
        ${cfg.dataDir}.d = {
          mode = "0750";
          user = "root";
          group = "root";
        };
      };

      # The media dirs are bind-mounted into the container at start, so the
      # mounts backing them have to be up first — a media dir that is still an
      # empty mountpoint gets baked into the container as an empty directory
      # and stays empty for the lifetime of the container.
      systemd.services.${serviceName} = {
        after = [
          "network-online.target"
          "nixflix-setup-dirs.service"
        ]
        ++ nixflix.serviceDependencies
        ++ optional nixflix.jellyfin.enable "jellyfin.service";
        wants = [ "network-online.target" ];
        requires = [ "nixflix-setup-dirs.service" ] ++ nixflix.serviceDependencies;
      };

      networking.firewall = mkIf cfg.openFirewall {
        allowedTCPPorts = [ cfg.port ];
        allowedUDPPorts = optional cfg.hdhr.openDiscoveryPort 1900;
      };

      services.nginx.virtualHosts.${hostname} = mkIf (nixflix.nginx.enable && cfg.reverseProxy.expose) {
        inherit (nixflix.nginx) forceSSL;
        useACMEHost = if nixflix.nginx.enableACME then nixflix.nginx.domain else null;

        locations."/" = {
          proxyPass = "http://127.0.0.1:${toString cfg.port}";
          recommendedProxySettings = true;
          # Tunarr pushes job/session updates over websockets, and its channel
          # streams are long-lived responses that must not be buffered.
          proxyWebsockets = true;
          extraConfig = ''
            proxy_redirect off;
            proxy_buffering off;
          '';
        };
      };

      services.caddy.virtualHosts."${caddyHostPrefix}${hostname}" =
        mkIf (nixflix.caddy.enable && cfg.reverseProxy.expose)
          {
            extraConfig = concatStringsSep "\n" (
              optional (nixflix.caddy.tls.enable && nixflix.caddy.tls.internal) "tls internal"
              ++ [
                ''
                  reverse_proxy http://127.0.0.1:${toString cfg.port} {
                    flush_interval -1
                  }
                ''
              ]
            );
          };

      networking.hosts = mkIf (
        cfg.reverseProxy.expose && nixflix.reverseProxy.enable && nixflix.reverseProxy.addHostsEntries
      ) { "127.0.0.1" = [ hostname ]; };
    }
  );
}
