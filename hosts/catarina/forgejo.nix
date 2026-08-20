{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.forgejo;
  domain = config.capivaras.fqdn "src";
  rootDomain = config.capivaras.url "src";
  email = config.capivaras.email "noreply";
in
{
  age.secrets.forgejo-secret-key = {
    file = ../../secrets/catarina/forgejo/secret-key.age;
    owner = cfg.user;
    group = cfg.group;
  };
  age.secrets.forgejo-internal-token = {
    file = ../../secrets/catarina/forgejo/internal-token.age;
    owner = cfg.user;
    group = cfg.group;
  };
  age.secrets.forgejo-admin-password = {
    file = ../../secrets/catarina/forgejo/admin-password.age;
    owner = cfg.user;
    group = cfg.group;
  };
  age.secrets.forgejo-mailer-password = {
    file = ../../secrets/catarina/smtp/noreply-password.age;
    owner = cfg.user;
    group = cfg.group;
  };
  age.secrets.forgejo-oidc-client-secret = {
    file = ../../secrets/catarina/authelia/forgejo-client-secret.age;
    owner = cfg.user;
    group = cfg.group;
  };

  services.forgejo = {
    enable = true;
    database.type = "postgres";
    settings = {
      DEFAULT = {
        APP_NAME = "capivaras";
        APP_SLOGAN = "personal code forge";
      };
      actions = {
        DEFAULT_ACTIONS_URL = rootDomain;
      };
      metrics = {
        ENABLED = true;
      };
      mirror = {
        DEFAULT_INTERVAL = "1h";
      };
      repository = {
        FORCE_PRIVATE = true;
      };
      "repository.pull-request" = {
        DEFAULT_MERGE_STYLE = "squash";
        DEFAULT_UPDATE_STYLE = "rebase";
      };
      server = {
        DOMAIN = domain;
        ROOT_URL = rootDomain;
        LANDING_PAGE = "explore";

        # Git-over-SSH is served by Forgejo's own built-in server on an internal
        # port, kept separate from the host's admin sshd. teresa's caddy-l4
        # forwards public :2222 here over Tailscale. SSH_DOMAIN/SSH_PORT are what
        # clone URLs advertise, so they name teresa's public endpoint.
        START_SSH_SERVER = true;
        SSH_LISTEN_PORT = 2222;
        SSH_DOMAIN = domain;
        SSH_PORT = 2222;
        SSH_USER = cfg.user;
      };
      service = {
        # Managed by authelia
        DISABLE_REGISTRATION = false;
        ALLOW_ONLY_EXTERNAL_REGISTRATION = true;

        ENABLE_NOTIFY_MAIL = true;
        DEFAULT_KEEP_EMAIL_PRIVATE = true;
        DEFAULT_USER_VISIBILITY = "private";
        DEFAULT_ORG_VISIBILITY = "private";
        # Set to false as reusable workflows can't be private.
        REQUIRE_SIGNIN_VIEW = false;
        ENABLE_INTERNAL_SIGNIN = false;
      };
      openid = {
        ENABLE_OPENID_SIGNIN = false;
      };
      oauth2_client = {
        ACCOUNT_LINKING = "login";
        ENABLE_AUTO_REGISTRATION = true;
        USERNAME = "preferred_username";
      };
      "service.explore" = {
        DISABLE_USERS_PAGE = true;
      };
      mailer = {
        ENABLED = true;
        PROTOCOL = "smtp+starttls";
        SMTP_ADDR = "smtp.purelymail.com";
        SMTP_PORT = 587;
        USER = email;
        FROM = "Forgejo <${email}>";
      };
      other = {
        SHOW_FOOTER_VERSION = false;
        ENABLE_SITEMAP = false;
        ENABLE_FEED = false;
      };
    };
    secrets = {
      mailer = {
        PASSWD = config.age.secrets.forgejo-mailer-password.path;
      };
      security = {
        SECRET_KEY = lib.mkForce config.age.secrets.forgejo-secret-key.path;
        INTERNAL_TOKEN = lib.mkForce config.age.secrets.forgejo-internal-token.path;
      };
    };
  };

  # agenix rewrites the file behind a stable /run/agenix path, so changing a
  # secret's *content* leaves this unit's text identical and systemd never
  # restarts it. Forgejo would then keep serving the credentials it read at its
  # last start. Hashing the ciphertext gives a trigger that moves only when the
  # secret really changes, unlike its store path, which moves on every commit.
  systemd.services.forgejo.restartTriggers = [
    (builtins.hashFile "sha256" ../../secrets/catarina/smtp/noreply-password.age)
  ];

  # setup the admin user on a new instance
  systemd.services.forgejo.preStart = lib.mkAfter (
    let
      adminCmd = "${lib.getExe cfg.package} admin user";
      authCmd = "${lib.getExe cfg.package} admin auth";
      pwd = config.age.secrets.forgejo-admin-password.path;
      clientSecret = config.age.secrets.forgejo-oidc-client-secret.path;
      user = "ratsclub";
      oauthName = "authelia";
    in
    ''
      ${adminCmd} create \
        --admin \
        --email "root@localhost" \
        --username ${user} \
        --password "$(tr -d '\n' < ${pwd})" || true

      # The OAuth2 source is a database row rather than app.ini, so it gets
      # reconciled here. add-oauth fails when the source already exists, and the
      # "|| true" idiom above would then silently ignore a rotated client secret,
      # so an existing source is updated in place instead.
      #
      # The source name is load-bearing: it forms the callback URL that Authelia
      # has in its redirect_uris.
      #
      # skip-local-2fa because Authelia already enforces a second factor; without
      # it Forgejo would ask for its own on top.
      oauthArgs=(
        --name ${oauthName}
        --provider openidConnect
        --key forgejo
        --secret "$(tr -d '\n' < ${clientSecret})"
        --auto-discover-url ${config.capivaras.url "auth"}/.well-known/openid-configuration
        --scopes openid --scopes email --scopes profile --scopes groups
        --skip-local-2fa
      )

      # Failures here are logged, never fatal. preStart runs under "set -e", so
      # letting these abort would mean an Authelia outage keeps Forgejo from
      # starting at all, which is exactly the coupling worth avoiding.
      oauthId=$(${authCmd} list | awk '$2 == "${oauthName}" { print $1 }' || true)
      if [ -n "$oauthId" ]; then
        ${authCmd} update-oauth --id "$oauthId" "''${oauthArgs[@]}" \
          || echo "warning: could not update the ${oauthName} oauth2 source" >&2
      else
        ${authCmd} add-oauth "''${oauthArgs[@]}" \
          || echo "warning: could not register the ${oauthName} oauth2 source" >&2
      fi
    ''
  );

  # Custom branding
  systemd.tmpfiles.rules =
    let
      img = "${cfg.customDir}/public/assets/img";
      logoSvg = ../../assets/img/shy-rat.svg;
      logoPng = ../../assets/img/shy-rat.png;
      robotsTxt = pkgs.writeText "forgejo-robots.txt" ''
        User-agent: *
        Disallow: /
      '';
    in
    [
      "d ${cfg.customDir}/public        0750 ${cfg.user} ${cfg.group} - -"
      "d ${cfg.customDir}/public/assets 0750 ${cfg.user} ${cfg.group} - -"
      "d ${img}                         0750 ${cfg.user} ${cfg.group} - -"
      "L+ ${cfg.customDir}/public/robots.txt - - - - ${robotsTxt}"
      "L+ ${img}/logo.svg    - - - - ${logoSvg}"
      "L+ ${img}/logo.png    - - - - ${logoPng}"
      "L+ ${img}/favicon.svg - - - - ${logoSvg}"
      "L+ ${img}/favicon.png - - - - ${logoPng}"
    ];

  services.restic.backups.forgejo = {
    initialize = true;

    repository = "b2:catarina-restic:forgejo";
    passwordFile = config.age.secrets.restic-password.path;
    environmentFile = config.age.secrets.restic-env.path;

    paths = [
      "/var/lib/forgejo"
      "/var/backup/forgejo/forgejo.sql"
    ];

    backupPrepareCommand = ''
      install -d -m 0700 /var/backup/forgejo
      ${pkgs.util-linux}/bin/runuser -u ${cfg.user} -- \
        ${config.services.postgresql.package}/bin/pg_dump --clean --if-exists \
        ${cfg.database.name} > /var/backup/forgejo/forgejo.sql
    '';

    backupCleanupCommand = ''
      rm -f /var/backup/forgejo/forgejo.sql
    '';

    pruneOpts = [
      "--keep-daily 7"
      "--keep-weekly 5"
      "--keep-monthly 12"
    ];

    timerConfig = {
      OnCalendar = "daily";
      Persistent = true;
    };
  };

  services.prometheus.scrapeConfigs = lib.mkIf cfg.enable [
    {
      job_name = "forgejo";
      static_configs = [ { targets = [ "127.0.0.1:3000" ]; } ];
      metrics_path = "/metrics";
    }
  ];

  systemd.services.restic-backups-forgejo = {
    after = [ "postgresql.service" ];
    requires = [ "postgresql.service" ];
  };
}
