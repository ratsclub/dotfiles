{
  config,
  pkgs,
  ...
}:

let
  cfg = config.services.grafana;
  statsUrl = config.capivaras.url "stats";
  issuer = config.capivaras.oidc.issuer;
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
  age.secrets.grafana-oidc-client-secret = {
    file = ../../secrets/catarina/authelia/grafana-client-secret.age;
    owner = "grafana";
    group = "grafana";
  };

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
            targets = [ "gemma:9100" ];
            labels.instance = "gemma";
          }
        ];
      }
      {
        job_name = "postgres";
        static_configs = [ { targets = [ "127.0.0.1:9187" ]; } ];
      }
      {
        job_name = "blocky";
        static_configs = [
          { targets = [ "127.0.0.1:${toString config.services.blocky.settings.ports.http}" ]; }
        ];
      }
      {
        job_name = "traefik";
        static_configs = [
          {
            targets = [ "gemma:8082" ];
            labels.instance = "gemma";
          }
        ];
      }
      {
        job_name = "caddy";
        static_configs = [
          {
            targets = [ "teresa:2020" ];
            labels.instance = "teresa";
          }
        ];
      }
    ];
  };

  services.prometheus.exporters.postgres = {
    enable = true;
    # Runs as the local 'postgres' OS user so it can connect via Unix socket
    # with peer authentication — no password or extra role needed.
    runAsLocalSuperUser = true;
    listenAddress = "127.0.0.1";
    port = 9187;
  };

  services.grafana = {
    enable = true;
    settings = {
      server = {
        http_addr = "0.0.0.0";
        http_port = 3001;
        domain = config.capivaras.fqdn "stats";
        root_url = "${statsUrl}/";
      };
      auth = {
        disable_login_form = true;
        signout_redirect_url = "${issuer}/logout";
      };
      "auth.generic_oauth" = {
        enabled = true;
        name = "Authelia";
        icon = "signin";
        auto_login = true;
        allow_sign_up = true;
        client_id = "grafana";
        client_secret = "$__file{${config.age.secrets.grafana-oidc-client-secret.path}}";
        scopes = "openid email profile groups";
        auth_url = "${issuer}/api/oidc/authorization";
        token_url = "${issuer}/api/oidc/token";
        api_url = "${issuer}/api/oidc/userinfo";
        use_pkce = true;
        login_attribute_path = "preferred_username";
        name_attribute_path = "name";
        groups_attribute_path = "groups";
        role_attribute_path = "contains(groups[*], 'grafana_admin') && 'GrafanaAdmin' || contains(groups[*], 'grafana_editor') && 'Editor'";
        allow_assign_grafana_admin = true;
        role_attribute_strict = false;
      };
      users.auto_assign_org_role = "Viewer";
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
            # Prometheus has no explicit global scrape_interval, so it uses the
            # 60s default. Grafana otherwise assumes 15s and resolves
            # $__rate_interval to ~60s — too short to hold 2 samples at a 60s
            # scrape, so every rate()-based panel returns "No data". Telling
            # Grafana the real interval makes $__rate_interval >= 4x it.
            jsonData.timeInterval = "60s";
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

    restartTriggers = [
      (builtins.hashFile "sha256" ../../secrets/catarina/authelia/grafana-client-secret.age)
    ];
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
