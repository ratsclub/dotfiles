{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.forgejo;
  domain = "src.r6b.dev";
  rootDomain = "https://${domain}";
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
    file = ../../secrets/catarina/forgejo/mailer-password.age;
    owner = cfg.user;
    group = cfg.group;
  };

  age.secrets.restic-password.file = ../../secrets/catarina/restic/password.age;
  age.secrets.restic-env.file = ../../secrets/catarina/restic/env.age;

  services.forgejo = {
    enable = true;
    database.type = "postgres";
    settings = {
      DEFAULT = {
        APP_NAME = "r6b";
        APP_SLOGAN = "personal code forge";
      };
      actions = {
        DEFAULT_ACTIONS_URL = rootDomain;
      };
      metrics = {
        ENABLED = true;
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
        DISABLE_REGISTRATION = true;
        ENABLE_NOTIFY_MAIL = true;
        DEFAULT_KEEP_EMAIL_PRIVATE = true;
        DEFAULT_USER_VISIBILITY = "private";
        DEFAULT_ORG_VISIBILITY = "private";
        # Set to false as reusable workflows can't be private.
        REQUIRE_SIGNIN_VIEW = false;
      };
      "service.explore" = {
        DISABLE_USERS_PAGE = true;
      };
      mailer = {
        ENABLED = true;
        PROTOCOL = "smtp+starttls";
        SMTP_ADDR = "smtp.purelymail.com";
        SMTP_PORT = 587;
        USER = "noreply@r6b.dev";
        FROM = "Forgejo <noreply@r6b.dev>";
      };
      other = {
        SHOW_FOOTER_VERSION = false;
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

  # setup the admin user on a new instance
  systemd.services.forgejo.preStart =
    let
      adminCmd = "${lib.getExe cfg.package} admin user";
      pwd = config.age.secrets.forgejo-admin-password.path;
      user = "ratsclub";
    in
    ''
      ${adminCmd} create \
        --admin \
        --email "root@localhost" \
        --username ${user} \
        --password "$(tr -d '\n' < ${pwd})" || true
    '';

  # Custom branding
  systemd.tmpfiles.rules =
    let
      img = "${cfg.customDir}/public/assets/img";
      logoSvg = ../../assets/img/shy-rat.svg;
      logoPng = ../../assets/img/shy-rat.png;
    in
    [
      "d ${cfg.customDir}/public        0750 ${cfg.user} ${cfg.group} - -"
      "d ${cfg.customDir}/public/assets 0750 ${cfg.user} ${cfg.group} - -"
      "d ${img}                         0750 ${cfg.user} ${cfg.group} - -"
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
