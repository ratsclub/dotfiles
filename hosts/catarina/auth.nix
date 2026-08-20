{
  config,
  lib,
  pkgs,
  ...
}:

let
  forgejoUrl = "https://${config.services.forgejo.settings.server.DOMAIN}";
in
{
  age.secrets.lldap-env.file = ../../secrets/catarina/lldap/env.age;
  age.secrets.lldap-admin-password.file = ../../secrets/catarina/lldap/admin-password.age;

  services.lldap = {
    enable = true;

    database = {
      createLocally = true;
      type = "postgresql";
    };

    # LLDAP_JWT_SECRET and LLDAP_KEY_SEED
    environmentFile = config.age.secrets.lldap-env.path;
    environment.LLDAP_LDAP_USER_PASS_FILE = "%d/user_pass";

    settings = {
      ldap_base_dn = "dc=capivaras,dc=dev";

      ldap_user_dn = "admin";
      ldap_user_email = "admin@capivaras.dev";

      force_ldap_user_pass_reset = "always";

      ldap_host = "::1";
      http_host = "0.0.0.0";
      http_url = "http://127.0.0.1:17170";
    };
  };

  systemd.services.lldap.serviceConfig.LoadCredential = [
    "user_pass:${config.age.secrets.lldap-admin-password.path}"
  ];

  age.secrets.authelia-jwt-secret.file = ../../secrets/catarina/authelia/jwt-secret.age;
  age.secrets.authelia-session-secret.file = ../../secrets/catarina/authelia/session-secret.age;
  age.secrets.authelia-storage-encryption-key.file = ../../secrets/catarina/authelia/storage-encryption-key.age;
  age.secrets.authelia-oidc-hmac-secret.file = ../../secrets/catarina/authelia/oidc-hmac-secret.age;
  age.secrets.authelia-oidc-issuer-private-key.file = ../../secrets/catarina/authelia/oidc-issuer-private-key.age;
  age.secrets.authelia-ldap-password.file = ../../secrets/catarina/authelia/ldap-password.age;
  age.secrets.authelia-smtp-password.file = ../../secrets/catarina/authelia/smtp-password.age;

  services.authelia.instances.main = {
    enable = true;

    # Empty name so the unit, user, group and database are all plainly "authelia".
    name = "";

    secrets = {
      jwtSecretFile = config.age.secrets.authelia-jwt-secret.path;
      sessionSecretFile = config.age.secrets.authelia-session-secret.path;
      storageEncryptionKeyFile = config.age.secrets.authelia-storage-encryption-key.path;
      oidcHmacSecretFile = config.age.secrets.authelia-oidc-hmac-secret.path;
      oidcIssuerPrivateKeyFile = config.age.secrets.authelia-oidc-issuer-private-key.path;
    };

    environmentVariables = {
      AUTHELIA_AUTHENTICATION_BACKEND_LDAP_PASSWORD_FILE = "%d/ldapPassword";
      AUTHELIA_NOTIFIER_SMTP_PASSWORD_FILE = "%d/smtpPassword";
    };

    settings = {
      theme = "auto";
      server.address = "tcp://0.0.0.0:9091";
      log.level = "info";
      authentication_backend = {
        ldap = {
          implementation = "lldap";
          address = "ldap://[::1]:3890";
          base_dn = "dc=capivaras,dc=dev";
          user = "uid=authelia,ou=people,dc=capivaras,dc=dev";
        };
      };

      access_control.default_policy = "two_factor";

      session.cookies = [
        {
          domain = "capivaras.dev";
          authelia_url = "https://auth.capivaras.dev";
        }
      ];

      storage.postgres = {
        address = "unix:///run/postgresql";
        database = "authelia";
        username = "authelia";
      };

      notifier.smtp = {
        address = "submission://smtp.purelymail.com:587";
        sender = "Authelia <noreply@capivaras.dev>";
        username = "noreply@capivaras.dev";
      };

      default_2fa_method = "totp";
      totp.issuer = "capivaras.dev";
      webauthn.display_name = "capivaras.dev";

      identity_providers.oidc.clients = [
        {
          client_id = "forgejo";
          client_name = "Forgejo";
          client_secret = "$pbkdf2-sha512$310000$AH8xBuL81nsKgDYZf/mtjA$RYaoDp8Tz9NcHiHq2VMiCM/fU.oe7mUn9HFdBV5T3o0leuFp9FIGrTq4a6O/TzREeZkRtujfLkxlM/feaKo0Zw";
          public = false;
          authorization_policy = "two_factor";
          require_pkce = true;
          pkce_challenge_method = "S256";
          redirect_uris = [ "${forgejoUrl}/user/oauth2/authelia/callback" ];
          scopes = [
            "openid"
            "email"
            "profile"
            "groups"
          ];
          response_types = [ "code" ];
          grant_types = [ "authorization_code" ];
          token_endpoint_auth_method = "client_secret_basic";
        }
      ];
    };
  };

  systemd.services.authelia.serviceConfig.LoadCredential = [
    "ldapPassword:${config.age.secrets.authelia-ldap-password.path}"
    "smtpPassword:${config.age.secrets.authelia-smtp-password.path}"
  ];

  services.postgresql = {
    ensureDatabases = [ "authelia" ];
    ensureUsers = [
      {
        name = "authelia";
        ensureDBOwnership = true;
      }
    ];
  };

  services.restic.backups.auth =
    let
      databases = [
        "lldap"
        "authelia"
      ];
    in
    {
      initialize = true;

      repository = "b2:catarina-restic:auth";
      passwordFile = config.age.secrets.restic-password.path;
      environmentFile = config.age.secrets.restic-env.path;

      paths = map (db: "/var/backup/auth/${db}.sql") databases;

      backupPrepareCommand = ''
        install -d -m 0700 /var/backup/auth
      ''
      + lib.concatMapStrings (db: ''
        ${pkgs.util-linux}/bin/runuser -u postgres -- \
          ${config.services.postgresql.package}/bin/pg_dump --clean --if-exists \
          ${db} > /var/backup/auth/${db}.sql
      '') databases;

      backupCleanupCommand = ''
        rm -f /var/backup/auth/*.sql
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

  systemd.services.restic-backups-auth = {
    after = [ "postgresql.service" ];
    requires = [ "postgresql.service" ];
  };
}
