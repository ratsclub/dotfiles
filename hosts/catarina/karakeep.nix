{
  config,
  pkgs,
  ...
}:

let
  karakeepUrl = config.capivaras.url "keep";
  port = 3002;
in
{
  age.secrets.karakeep-oidc-client-secret.file = ../../secrets/catarina/authelia/karakeep-client-secret.age;
  age.secrets.karakeep-deepseek-api-key.file = ../../secrets/catarina/karakeep/deepseek-api-key.age;

  services.karakeep = {
    enable = true;

    environmentFile = config.age.secrets.karakeep-oidc-client-secret.path;

    extraEnvironment = {
      PORT = toString port;
      NEXTAUTH_URL = karakeepUrl;
      DISABLE_NEW_RELEASE_CHECK = "true";
      DISABLE_PASSWORD_AUTH = "true";
      CRAWLER_FULL_PAGE_ARCHIVE = "true";
      OAUTH_PROVIDER_NAME = "Authelia";
      OAUTH_WELLKNOWN_URL = config.capivaras.oidc.discoveryEndpoint;
      OAUTH_CLIENT_ID = "karakeep";
      OAUTH_AUTO_REDIRECT = "true";
      OAUTH_ALLOW_DANGEROUS_EMAIL_ACCOUNT_LINKING = "true";

      OPENAI_BASE_URL = "https://api.deepseek.com";
      INFERENCE_TEXT_MODEL = "deepseek-v4-flash";
      INFERENCE_IMAGE_MODEL = "deepseek-v4-flash-vision-exp";
      INFERENCE_OUTPUT_SCHEMA = "json";
      INFERENCE_CONTEXT_LENGTH = "16000";
      INFERENCE_JOB_TIMEOUT_SEC = "120";
      EMBEDDING_ENABLE_AUTO_INDEXING = "false";
      INFERENCE_ENABLE_AUTO_SUMMARIZATION = "true";
    };
  };

  # services.karakeep.environmentFile holds a single path, already spent on the
  # OIDC secret, so the API key rides in as a second EnvironmentFile. systemd
  # unit options merge lists by concatenation, so both files are read.
  systemd.services.karakeep-web.serviceConfig.EnvironmentFile = [
    config.age.secrets.karakeep-deepseek-api-key.path
  ];

  systemd.services.karakeep-workers.serviceConfig.EnvironmentFile = [
    config.age.secrets.karakeep-deepseek-api-key.path
  ];

  systemd.services.karakeep-web.restartTriggers = [
    (builtins.hashFile "sha256" ../../secrets/catarina/authelia/karakeep-client-secret.age)
    (builtins.hashFile "sha256" ../../secrets/catarina/karakeep/deepseek-api-key.age)
  ];

  systemd.services.karakeep-workers.restartTriggers = [
    (builtins.hashFile "sha256" ../../secrets/catarina/authelia/karakeep-client-secret.age)
    (builtins.hashFile "sha256" ../../secrets/catarina/karakeep/deepseek-api-key.age)
  ];

  services.restic.backups.karakeep = {
    initialize = true;

    repository = "b2:catarina-restic:karakeep";
    passwordFile = config.age.secrets.restic-password.path;
    environmentFile = config.age.secrets.restic-env.path;

    paths = [
      # Backup the SQLite database using the `.backup` command so that
      # half-pages don't get into the backup.
      "/var/backup/karakeep/db.db"
      "/var/lib/karakeep"
    ];

    exclude = [ "/var/lib/karakeep/db.db*" ];

    backupPrepareCommand = ''
      install -d -m 0700 -o karakeep -g karakeep /var/backup/karakeep
      ${pkgs.util-linux}/bin/runuser -u karakeep -- \
        ${pkgs.sqlite}/bin/sqlite3 /var/lib/karakeep/db.db \
        ".backup '/var/backup/karakeep/db.db'"
    '';

    backupCleanupCommand = ''
      rm -f /var/backup/karakeep/db.db
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
}
