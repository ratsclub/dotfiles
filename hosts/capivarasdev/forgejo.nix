{ config, pkgs, ... }:

let
  fqdn = config.networking.fqdn;
  cfg = config.services.forgejo;
  srv = cfg.settings.server;
in
{
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

      actions.ENABLED = false;

      service.DISABLE_REGISTRATION = true;

      server.HTTP_ADDR = "127.0.0.1";
      server.DOMAIN = "code.${fqdn}";
      server.ROOT_URL = "https://${srv.DOMAIN}";

      mailer.ENABLED = true;
      mailer.SMTP_ADDR = "smtp.purelymail.com";
      mailer.SMTP_PORT = 587;
      mailer.PROTOCOL = "smtp+starttls";
      mailer.FROM = "noreply@${fqdn}";
      mailer.USER = "noreply@${fqdn}";
    };
    mailerPasswordFile = config.age.secrets.forgejomail.path;
  };
}
