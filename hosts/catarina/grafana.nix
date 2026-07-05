{
  config,
  pkgs,
  ...
}:

let
  cfg = config.services.grafana;
in
{
  age.secrets.grafana-admin-password = {
    file = ../../secrets/catarina/grafana/admin-password.age;
    owner = "grafana";
    group = "grafana";
  };
  age.secrets.grafana-secret-key = {
    file = ../../secrets/catarina/grafana/secret-key.age;
    owner = "grafana";
    group = "grafana";
  };

  # Prometheus scrapes node-exporter on both hosts. joan is reachable over
  # Tailscale MagicDNS; swap for the full joan.<tailnet>.ts.net name if the short
  # name doesn't resolve.
  services.prometheus = {
    enable = true;
    listenAddress = "127.0.0.1";
    port = 9090;
    scrapeConfigs = [
      {
        job_name = "node";
        static_configs = [
          {
            targets = [ "127.0.0.1:9100" ];
            labels.instance = "catarina";
          }
          {
            targets = [ "joan:9100" ];
            labels.instance = "joan";
          }
        ];
      }
    ];
  };

  services.grafana = {
    enable = true;
    settings = {
      server = {
        http_addr = "0.0.0.0";
        http_port = 3001;
        domain = "catarina";
      };
      security = {
        admin_user = "admin";
        # admin_password only seeds the account when the grafana DB is first
        # created; rotating admin-password.age later does NOT change the live
        # password. Reset it on the host with:
        #   grafana-cli admin reset-admin-password <new-password>
        admin_password = "$__file{${config.age.secrets.grafana-admin-password.path}}";
        # Used to sign auth cookies / dashboard snapshot URLs. No default anymore.
        secret_key = "$__file{${config.age.secrets.grafana-secret-key.path}}";
      };
      # State lives in the host's PostgreSQL over the local socket with peer auth
      # (the grafana OS user maps to the grafana postgres role, so no password).
      database = {
        type = "postgres";
        host = "/run/postgresql";
        name = "grafana";
        user = "grafana";
      };
    };
    provision = {
      enable = true;
      datasources.settings = {
        datasources = [
          {
            name = "Prometheus";
            # Pinned so the provisioned dashboard JSON can reference this
            # datasource by a stable uid instead of Grafana's generated one.
            uid = "prometheus";
            type = "prometheus";
            url = "http://127.0.0.1:9090";
            isDefault = true;
          }
        ];
        # The first deploy created "Prometheus" with an auto-generated uid.
        # Provisioning can't retag an existing datasource with the pinned uid
        # ("data source not found"), so delete it first and let the block above
        # recreate it with uid "prometheus". Idempotent on later restarts.
        deleteDatasources = [
          {
            name = "Prometheus";
            orgId = 1;
          }
        ];
      };
      # Dashboards live as JSON in ./dashboards and are provisioned read-only;
      # edit them in the repo, not the Grafana UI.
      dashboards.settings.providers = [
        {
          name = "default";
          options.path = ./dashboards;
        }
      ];
    };
  };

  services.postgresql = {
    ensureDatabases = [ "grafana" ];
    ensureUsers = [
      {
        name = "grafana";
        ensureDBOwnership = true;
      }
    ];
  };

  systemd.services.grafana = {
    after = [ "postgresql.service" ];
    requires = [ "postgresql.service" ];
  };

  services.restic.backups.grafana = {
    initialize = true;

    repository = "b2:catarina-restic:grafana";
    passwordFile = config.age.secrets.restic-password.path;
    environmentFile = config.age.secrets.restic-env.path;

    paths = [
      "/var/backup/grafana/grafana.sql"
    ];

    backupPrepareCommand = ''
      install -d -m 0700 /var/backup/grafana
      ${pkgs.util-linux}/bin/runuser -u ${cfg.settings.database.user} -- \
        ${config.services.postgresql.package}/bin/pg_dump --clean --if-exists \
        ${cfg.settings.database.name} > /var/backup/grafana/grafana.sql
    '';

    backupCleanupCommand = ''
      rm -f /var/backup/grafana/grafana.sql
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

  systemd.services.restic-backups-grafana = {
    after = [ "postgresql.service" ];
    requires = [ "postgresql.service" ];
  };
}
