{
  config,
  pkgs,
  ...
}:

let
  inherit (pkgs) lib;
  pagesDomain = "capivaras.page";
  forgeUrl = lib.removeSuffix "/" config.services.forgejo.settings.server.ROOT_URL;

  port = 3003;
  metricsPort = 3004;

  settings = {
    log-format = "text";

    server = {
      pages = "tcp/0.0.0.0:${toString port}";
      metrics = "tcp/127.0.0.1:${toString metricsPort}";
      # Disables caddy endpoint as traefik is used instead.
      caddy = "-";
    };

    storage = {
      type = "fs";
      fs.root = "/var/lib/git-pages";
    };

    wildcard = [
      {
        domain = pagesDomain;
        clone-url = "${forgeUrl}/<user>/<project>.git";
        index-repo = "<user>.${pagesDomain}";
        index-repo-branch = "main";
        authorization = "forgejo";
      }
    ];

    limits = {
      allowed-repository-url-prefixes = [ "${forgeUrl}/" ];
    };
  };

  configFile = (pkgs.formats.toml { }).generate "git-pages.toml" settings;
in
{
  users.users.git-pages = {
    isSystemUser = true;
    group = "git-pages";
  };

  users.groups.git-pages = { };

  systemd.services.git-pages = {
    description = "git-pages static site server";
    wantedBy = [ "multi-user.target" ];
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];

    serviceConfig = {
      ExecStart = "${lib.getExe pkgs.git-pages} -config ${configFile}";
      Restart = "on-failure";
      RestartSec = "5s";

      User = "git-pages";
      Group = "git-pages";

      # An empty root on the runtime tmpfs: the service sees the three paths
      # bound below and nothing else of this machine.
      RuntimeDirectory = "git-pages";
      RootDirectory = "/run/git-pages";
      StateDirectory = "git-pages";
      StateDirectoryMode = "0700";
      WorkingDirectory = "/var/lib/git-pages";

      BindPaths = [ "/var/lib/git-pages" ];
      BindReadOnlyPaths = [
        builtins.storeDir
        "${config.security.pki.caBundle}:/etc/ssl/certs/ca-certificates.crt"
        "-/etc/resolv.conf"
      ];

      AmbientCapabilities = "";
      CapabilityBoundingSet = "";
      LockPersonality = true;
      MemoryDenyWriteExecute = true;
      NoNewPrivileges = true;
      PrivateDevices = true;
      PrivateIPC = true;
      PrivateMounts = true;
      PrivatePIDs = true;
      PrivateTmp = true;
      PrivateUsers = true;
      ProtectClock = true;
      ProtectControlGroups = "strict";
      ProtectHome = true;
      ProtectHostname = true;
      ProtectKernelLogs = true;
      ProtectKernelModules = true;
      ProtectKernelTunables = true;
      ProtectProc = "invisible";
      ProtectSystem = "strict";
      RemoveIPC = true;
      RestrictNamespaces = true;
      RestrictRealtime = true;
      RestrictSUIDSGID = true;
      SystemCallArchitectures = "native";
      SystemCallErrorNumber = "EPERM";
      SystemCallFilter = [ "@system-service" ];
      UMask = "0077";

      RestrictAddressFamilies = [
        "AF_INET"
        "AF_INET6"
        "AF_NETLINK"
        "AF_UNIX"
      ];

      SocketBindDeny = "any";
      SocketBindAllow = [
        "tcp:${toString port}"
        "tcp:${toString metricsPort}"
      ];
    };
  };

  services.prometheus.scrapeConfigs = [
    {
      job_name = "git-pages";
      static_configs = [ { targets = [ "127.0.0.1:${toString metricsPort}" ]; } ];
    }
  ];

  services.restic.backups.git-pages = {
    initialize = true;

    repository = "b2:catarina-restic:git-pages";
    passwordFile = config.age.secrets.restic-password.path;
    environmentFile = config.age.secrets.restic-env.path;

    # Sites published from a repository can be reconstructed by pushing again,
    # but sites uploaded as an archive from a workflow cannot: the built output
    # only ever existed here.
    paths = [ "/var/lib/git-pages" ];

    pruneOpts = [
      "--keep-daily 7"
      "--keep-weekly 5"
      "--keep-monthly 12"
    ];

    timerConfig = {
      OnCalendar = "daily";
      RandomizedDelaySec = "1h";
      Persistent = true;
    };
  };
}
