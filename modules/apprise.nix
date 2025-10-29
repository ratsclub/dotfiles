{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.systemd.apprise;

  checkConditions = pkgs.writeScript "checkConditions" ''
    #!/bin/sh
    STATUS=$(systemctl status --full "$1")

    case "$STATUS" in
      *"activating (auto-restart) (Result: timeout)"*) exit 1 ;;
      *) exit 0 ;;
    esac
  '';

  apprise = pkgs.writeScript "apprise" ''
    #! ${pkgs.runtimeShell}

    apprise -vv \
      -t "Status of service $2" \
      -b "$(systemctl status --full $2)" \
      --config=${cfg.configFile}
  '';
in

{
  options = {
    systemd.services = mkOption {
      type =
        with types;
        attrsOf (submodule {
          config.onFailure = [ "apprise@%n.service" ];
        });
    };

    systemd.apprise = {
      enable = mkOption {
        default = false;
        type = types.bool;
        description = "Enable Apprise systemd notifications.";
      };

      configFile = mkOption {
        type = types.nullOr types.path;
        default = null;
        description = "The Apprise configuration file.";
      };
    };
  };

  config = mkIf cfg.enable {
    systemd.services."apprise@" = {
      description = "Sends a status notification via apprise on service failures.";
      onFailure = mkForce [ ];
      unitConfig = {
        StartLimitIntervalSec = "5m";
        StartLimitBurst = 1;
      };
      path = [ pkgs.apprise ];
      serviceConfig = {
        ExecCondition = "${checkConditions} %i";
        ExecStart = "${apprise} %i";
        Type = "oneshot";
      };
    };
  };
}
