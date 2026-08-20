{ config, lib, ... }:

let
  cfg = config.capivaras;
in
{
  options.capivaras = {
    domain = lib.mkOption {
      type = lib.types.str;
      default = "capivaras.dev";
      description = "Root domain every capivaras service hangs off of.";
    };

    baseDN = lib.mkOption {
      type = lib.types.str;
      readOnly = true;
      description = "LDAP base DN derived from the root domain.";
    };

    fqdn = lib.mkOption {
      type = lib.types.functionTo lib.types.str;
      readOnly = true;
      description = "Turns a service name into its fully qualified domain name.";
      example = lib.literalExpression ''config.capivaras.fqdn "auth" # auth.capivaras.dev'';
    };

    url = lib.mkOption {
      type = lib.types.functionTo lib.types.str;
      readOnly = true;
      description = "Turns a service name into its public HTTPS URL.";
      example = lib.literalExpression ''config.capivaras.url "auth" # https://auth.capivaras.dev'';
    };

    email = lib.mkOption {
      type = lib.types.functionTo lib.types.str;
      readOnly = true;
      description = "Turns a mailbox name into an address on the root domain.";
      example = lib.literalExpression ''config.capivaras.email "noreply" # noreply@capivaras.dev'';
    };

    oidc = {
      issuer = lib.mkOption {
        type = lib.types.str;
        readOnly = true;
        description = ''
          Authelia's public URL, which doubles as the OpenID Provider issuer.
          Every OIDC client on the estate points at this.
        '';
      };

      discoveryEndpoint = lib.mkOption {
        type = lib.types.str;
        readOnly = true;
        description = ''
          The provider's discovery document. Only for clients that want the full
          URL; libraries built on go-oidc take the bare issuer and append this
          path themselves.
        '';
      };
    };
  };

  config.capivaras = {
    baseDN = lib.concatMapStringsSep "," (part: "dc=${part}") (lib.splitString "." cfg.domain);
    fqdn = name: "${name}.${cfg.domain}";
    url = name: "https://${cfg.fqdn name}";
    email = name: "${name}@${cfg.domain}";

    oidc = {
      issuer = cfg.url "auth";
      discoveryEndpoint = "${cfg.oidc.issuer}/.well-known/openid-configuration";
    };
  };
}
