{
  config,
  pkgs,
  ...
}:

let
  minifluxUrl = config.capivaras.url "feed";
  port = 8081;
in
{
  age.secrets.miniflux-oidc-client-secret.file = ../../secrets/catarina/authelia/miniflux-client-secret.age;

  services.miniflux = {
    enable = true;

    config = {
      BASE_URL = minifluxUrl;
      LISTEN_ADDR = "0.0.0.0:${toString port}";

      CREATE_ADMIN = false;
      DISABLE_LOCAL_AUTH = 1;

      OAUTH2_PROVIDER = "oidc";
      OAUTH2_OIDC_PROVIDER_NAME = "Authelia";
      OAUTH2_OIDC_DISCOVERY_ENDPOINT = config.capivaras.oidc.issuer;
      OAUTH2_CLIENT_ID = "miniflux";
      OAUTH2_CLIENT_SECRET_FILE = "%d/oidcClientSecret";
      OAUTH2_REDIRECT_URL = "${minifluxUrl}/oauth2/oidc/callback";
      OAUTH2_USER_CREATION = 1;

      METRICS_COLLECTOR = 1;
      METRICS_ALLOWED_NETWORKS = "127.0.0.1/32";
    };
  };

  systemd.services.miniflux.serviceConfig.LoadCredential = [
    "oidcClientSecret:${config.age.secrets.miniflux-oidc-client-secret.path}"
  ];

  systemd.services.miniflux.restartTriggers = [
    (builtins.hashFile "sha256" ../../secrets/catarina/authelia/miniflux-client-secret.age)
  ];

  services.prometheus.scrapeConfigs = [
    {
      job_name = "miniflux";
      static_configs = [ { targets = [ "127.0.0.1:${toString port}" ]; } ];
      metrics_path = "/metrics";
    }
  ];

  services.restic.backups.miniflux = {
    initialize = true;

    repository = "b2:catarina-restic:miniflux";
    passwordFile = config.age.secrets.restic-password.path;
    environmentFile = config.age.secrets.restic-env.path;

    paths = [ "/var/backup/miniflux/miniflux.sql" ];

    backupPrepareCommand = ''
      install -d -m 0700 /var/backup/miniflux
      ${pkgs.util-linux}/bin/runuser -u postgres -- \
        ${config.services.postgresql.package}/bin/pg_dump --clean --if-exists \
        miniflux > /var/backup/miniflux/miniflux.sql
    '';

    backupCleanupCommand = ''
      rm -f /var/backup/miniflux/miniflux.sql
    '';

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

  systemd.services.restic-backups-miniflux = {
    after = [ "postgresql.service" ];
    requires = [ "postgresql.service" ];
  };
}
