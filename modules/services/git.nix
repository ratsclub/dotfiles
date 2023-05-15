{ config, lib, pkgs, ... }:

let
  inherit (lib)
    concatStrings
    escapeShellArg
    mkEnableOption
    mkIf
    mkOption
    optionalAttrs
    strings
    types;

  inherit (strings) normalizePath;

  inherit (builtins) filter map;

  inherit (pkgs)
    gitMinimal
    shellcheck;

  service = "git";

  cfg = config.gluer.services."${service}";

  mkRepoScript = repo:
    let
      path = normalizePath "${cfg.dataDir}/${escapeShellArg repo.name}.git";
    in
    ''
      # safe to run on an existing repo
      git init --quiet --bare ${path}

      # add repository description
      echo "${repo.description}" > ${path}/description
    '';

  createRepositoriesScript =
    let
      repos =
        filter (repo: repo.name != null) cfg.repositories;
    in
    concatStrings (
      map (repo: mkRepoScript repo) repos
    );
in
{
  options.gluer.services."${service}" = {
    enable = mkEnableOption (lib.mdDoc "gluer ${service}");

    dataDir = mkOption {
      type = types.path;
      default = "/var/lib/${service}";
      description = lib.mdDoc "The directory where cgit will look for repositories.";
    };

    user = mkOption {
      type = types.str;
      default = service;
      description = lib.mdDoc "User under which cgit will be running.";
    };

    group = mkOption {
      type = types.str;
      default = service;
      description = lib.mdDoc "Group under which cgit will be running.";
    };

    repositories = mkOption {
      description = lib.mdDoc "git repositories that will be automatically created.";
      default = { };
      type = types.listOf (types.submodule {
        options = {
          name = mkOption {
            type = types.nullOr types.str;
            default = null;
            description = "Repository path";
          };
          description = mkOption {
            type = types.str;
            default = "";
            description = "Repository description";
          };
        };
      });
    };

    authorizedKeys = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = lib.mdDoc "Authorized keys for the cgit user.";
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ gitMinimal ];

    # create the service user
    users = optionalAttrs (cfg.user == service && cfg.group == service) {
      groups."${cfg.user}" = { };
      users."${cfg.user}" = {
        inherit (cfg) group;
        createHome = true;
        home = cfg.dataDir;
        isSystemUser = true;
        shell = "${pkgs.git}/bin/git-shell";
        openssh.authorizedKeys.keys = cfg.authorizedKeys;
      };
    };

    # create directories with the correct permissions
    systemd.tmpfiles.rules = [
      "d ${cfg.dataDir} 770 ${cfg.user} ${cfg.group} - -"
    ];

    systemd.services = {
      "gluer-${service}-setup" = {
        description = "Automatically create git repositories";
        after = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];
        path = [ gitMinimal shellcheck ];
        script = ''
          set -euxo pipefail
          shellcheck "$0" || exit 1

          ${createRepositoriesScript}
        '';

        serviceConfig = {
          Type = "oneshot";
          User = cfg.user;
          Group = cfg.group;
          WorkingDirectory = cfg.dataDir;
          RemainAfterExit = true;
        };
      };
    };
  };
}

