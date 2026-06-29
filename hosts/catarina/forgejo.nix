{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.forgejo;
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

  age.secrets.restic-password.file = ../../secrets/catarina/restic/password.age;
  age.secrets.restic-env.file = ../../secrets/catarina/restic/env.age;

  services.forgejo = {
    enable = true;
    database.type = "postgres";
    settings = {
      server = {
        DOMAIN = "catarina";
        ROOT_URL = "http://${cfg.settings.server.DOMAIN}:${toString cfg.settings.server.HTTP_PORT}/";
      };
      service = {
        DISABLE_REGISTRATION = true;
      };
      other = {
        SHOW_FOOTER_VERSION = false;
      };
    };
    secrets.security = {
      SECRET_KEY = lib.mkForce config.age.secrets.forgejo-secret-key.path;
      INTERNAL_TOKEN = lib.mkForce config.age.secrets.forgejo-internal-token.path;
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

  systemd.services.restic-backups-forgejo = {
    after = [ "postgresql.service" ];
    requires = [ "postgresql.service" ];
  };
}
