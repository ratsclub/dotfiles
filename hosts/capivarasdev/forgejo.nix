{ config, pkgs, ... }:

let
  fqdn = config.networking.fqdn;
  cfg = config.services.forgejo;
  srv = cfg.settings.server;
in
{
  age.secrets = {
    forgejo-mailer.file = ../../secrets/services/forgejo/mailer.age;
    forgejo-mailer.owner = config.services.forgejo.user;
    forgejo-mailer.group = config.services.forgejo.group;
  };

  services.caddy = {
    virtualHosts."${srv.DOMAIN}".extraConfig = ''
      reverse_proxy localhost:${builtins.toString srv.HTTP_PORT}
    '';
  };

  services.postgresql = {
    ensureDatabases = [ cfg.database.name ];
    ensureUsers = [
      {
        name = cfg.database.user;
        ensureDBOwnership = true;
      }
    ];
  };

  services.forgejo = {
    enable = true;
    package = pkgs.forgejo;
    database.type = "postgres";

    dump.enable = true;
    dump.interval = "daily";

    settings = {
      DEFAULT.APP_NAME = "capivaras.dev code forge";

      actions.ENABLED = true;
      actions.ARTIFACT_RETENTION_DAYS = 30;

      service.DISABLE_REGISTRATION = true;

      server.HTTP_ADDR = "127.0.0.1";
      server.DOMAIN = "code.${fqdn}";
      server.ROOT_URL = "https://${srv.DOMAIN}";
      server.LANDING_PAGE = "/explore/repos";

      mailer.ENABLED = true;
      mailer.SMTP_ADDR = "smtp.purelymail.com";
      mailer.SMTP_PORT = 587;
      mailer.PROTOCOL = "smtp+starttls";
      mailer.FROM = "noreply@${srv.DOMAIN}";
      mailer.USER = "noreply@${srv.DOMAIN}";
    };
    mailerPasswordFile = config.age.secrets.forgejo-mailer.path;
  };
}
