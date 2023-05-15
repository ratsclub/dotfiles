{ config, lib, pkgs, ... }:

let
  inherit (lib)
    mkEnableOption
    mkOption
    mkIf
    optionalAttrs
    types;

  service = "legit";

  cfg = config.gluer.services.legit;

  yaml = pkgs.formats.yaml { };
  configFile = yaml.generate "legit.yaml" cfg.settings;

  dataDir = cfg.settings.repo.scanPath;
in
{
  options.gluer.services.${service} = {
    enable = mkEnableOption (lib.mdDoc "gluer ${service}");

    package = mkOption {
      type = types.package;
      default = pkgs.legit-web;
    };

    user = mkOption {
      type = types.str;
      default = service;
    };

    group = mkOption {
      type = types.str;
      default = service;
    };

    settings = mkOption {
      type = types.submodule {
        options.repo = {
          scanPath = mkOption {
            type = types.path;
            default = config.gluer.services.git.dataDir;
          };
          readme = mkOption {
            type = types.listOf types.str;
            default = [ ];
          };
          mainBranch = mkOption {
            type = types.listOf types.str;
            default = [ "main" "master" ];
          };
          ignore = mkOption {
            type = types.listOf types.str;
            default = [ ];
          };
        };
        options.dirs = {
          templates = mkOption {
            type = types.path;
            default = "${dataDir}/templates";
          };
          static = mkOption {
            type = types.path;
            default = "${dataDir}/description";
          };
        };
        options.meta = {
          title = mkOption {
            type = types.str;
            default = "legit";
          };
          description = mkOption {
            type = types.str;
            default = "git frontend";
          };
        };
        options.server = {
          name = mkOption {
            type = types.str;
            default = "git.${config.networking.domain}";
          };
          host = mkOption {
            type = types.str;
            default = "127.0.0.1";
          };
          port = mkOption {
            type = types.port;
            default = 9000;
          };
        };
      };
    };
  };

  config = mkIf cfg.enable {
    users.groups = optionalAttrs (cfg.group == service) {
      "${cfg.group}" = { };
    };

    users.users = optionalAttrs (cfg.user == service) {
      "${cfg.user}" = {
        group = cfg.group;
        isSystemUser = true;
      };
    };

    systemd.services."${service}" = {
      description = "${service} git frontend";

      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      restartTriggers = [ configFile ];

      serviceConfig = {
        Type = "simple";
        User = cfg.user;
        Group = cfg.group;
        WorkingDirectory = dataDir;
        ExecStart = "${cfg.package}/bin/legit -config ${configFile}";
        Restart = "on-failure";

        ProtectSystem = "strict";
        ProtectHome = "true";
        NoNewPrivileges = true;
        PrivateTmp = true;
        PrivateDevices = true;
      };
    };
  };
}
