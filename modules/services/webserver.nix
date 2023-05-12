{ config, lib, ... }:

let
  service = "webserver";

  cfg = config.gluer.services."${service}";

  inherit (lib)
    foldr
    recursiveUpdate
    mkEnableOption
    mkIf
    mkOption
    types;

  generateWebsiteRoot = website:
    "${cfg.dataDir}/${website.domain}";
in
{
  options.gluer.services."${service}" = {
    enable = mkEnableOption (lib.mdDoc "gluer ${service}");

    dataDir = mkOption {
      type = types.path;
      default = "/var/lib/www";
      description = lib.mdDoc "The directory where ${service} will look for static websites.";
    };

    user = mkOption {
      type = types.str;
      default = config.services.nginx.group;
      description = lib.mdDoc "User account under which dataDir will be created.";
    };

    group = mkOption {
      type = types.str;
      default = config.services.nginx.group;
      description = lib.mdDoc "Group account under which dataDir will be created.";
    };

    websites = mkOption {
      default = [ ];
      description = lib.mdDoc "Websites that will be setup for static hosting.";
      type = types.listOf (types.submodule {
        options = {
          # TODO: trim domain string
          domain = mkOption {
            type = types.nonEmptyStr;
            default = null;
            description = lib.mdDoc "Website domain.";
          };
        };
      });
    };
  };

  config = mkIf cfg.enable {
    systemd.tmpfiles.rules = [
      "d ${cfg.dataDir} 770 ${cfg.user} ${cfg.group} - -"
    ]
    ++ map
      (website:
        "d ${generateWebsiteRoot website} 770 ${cfg.user} ${cfg.group} - -")
      cfg.websites;

    services.nginx = {
      enable = true;
      virtualHosts =
        foldr
          (website: acc:
            recursiveUpdate
              {
                "${website.domain}" = {
                  forceSSL = true;
                  enableACME = true;
                  root = "${generateWebsiteRoot website}";
                };
              }
              acc)
          { }
          cfg.websites;
    };

    security.acme.acceptTerms = true;
  };
}
