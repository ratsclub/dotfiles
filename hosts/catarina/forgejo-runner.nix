{
  config,
  lib,
  pkgs,
  ...
}:

let
  bridge = "microvm";
  hostAddress = "10.100.0.1";
  guestAddress = "10.100.0.10";
  tapId = "vm-runner";
  mac = "02:00:00:00:00:01";

  secretsDir = "/run/microvm/secrets/runner";
  secretsMount = "/secrets";

  stateDir = "${config.microvm.stateDir}/runner";
  storeOverlayImage = "nix-store-overlay.img";

  forgejoPort = config.services.forgejo.settings.server.HTTP_PORT;
  forgejoUrl = "http://${hostAddress}:${toString forgejoPort}";
  forgejoDomain = config.services.forgejo.settings.server.DOMAIN;
  sshKeys = config.users.users.root.openssh.authorizedKeys.keys;
  hostTimeZone = config.time.timeZone;
in
{
  age.secrets.forgejo-runner-token = {
    file = ../../secrets/catarina/forgejo/runner-token.age;
    path = "${secretsDir}/token";
    # The secret is shared through a mount
    symlink = false;
  };

  networking.firewall.interfaces.${bridge}.allowedTCPPorts = [ forgejoPort ];
  services.forgejo.settings.actions.DEFAULT_ACTIONS_URL = lib.mkForce forgejoUrl;

  # Avoid conflicts between resolvd and blocky
  services.resolved.enable = false;

  systemd.network = {
    enable = true;
    wait-online.enable = false;

    netdevs."20-microvm".netdevConfig = {
      Kind = "bridge";
      Name = bridge;
    };

    networks."20-microvm" = {
      matchConfig.Name = bridge;
      addresses = [ { Address = "${hostAddress}/24"; } ];
      # The bridge has no carrier until the VM attaches its tap.
      networkConfig.ConfigureWithoutCarrier = true;
    };

    networks."21-microvm-tap" = {
      matchConfig.Name = "vm-*";
      networkConfig.Bridge = bridge;
    };
  };

  # Otherwise NetworkManager grabs the bridge and the tap and fights networkd.
  networking.networkmanager.unmanaged = [
    "interface-name:${bridge}"
    "interface-name:vm-*"
  ];

  networking.nat = {
    enable = true;
    externalInterface = "enp3s0";
    internalInterfaces = [ bridge ];
  };

  # The guest's Nix database lives on its ephemeral root, so a store overlay
  # that outlived a boot would hold paths Nix no longer knows about. Drop it
  # before every start and let autoCreate make a fresh one.
  systemd.services.microvm-runner-store-reset = {
    description = "Discard the runner MicroVM's writable Nix store overlay";
    before = [ "microvm@runner.service" ];
    requiredBy = [ "microvm@runner.service" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${lib.getExe' pkgs.coreutils "rm"} -f ${stateDir}/${storeOverlayImage}";
    };
  };

  microvm.vms.runner.config =
    { config, pkgs, ... }:
    let
      image = pkgs.forgejo-runner-image;
      tools = pkgs.forgejo-runner-tools;

      cachePort = 42000;
      proxyPort = 42001;

      containerVolumes = [
        {
          host = "/nix";
          guest = "/nix";
        }
        {
          host = "${tools}/bin";
          guest = "/bin";
        }
      ];
    in
    {
      imports = [ ../../modules/common/openssh.nix ];

      microvm = {
        hypervisor = "cloud-hypervisor";
        vcpu = 4;
        mem = 4096;
        vsock.cid = 3;

        shares = [
          {
            source = "/nix/store";
            mountPoint = "/nix/.ro-store";
            tag = "ro-store";
            proto = "virtiofs";
            readOnly = true;
          }
          {
            source = secretsDir;
            mountPoint = secretsMount;
            tag = "runner-secrets";
            proto = "virtiofs";
            readOnly = true;
          }
        ];

        interfaces = [
          {
            type = "tap";
            id = tapId;
            inherit mac;
          }
        ];

        volumes = [
          {
            image = "data.img";
            label = "runner-var";
            mountPoint = "/var";
            size = 30 * 1024;
          }
          {
            image = storeOverlayImage;
            label = "runner-rwstore";
            mountPoint = "/nix/.rw-store";
            size = 20 * 1024;
            mkfsExtraArgs = [
              "-O"
              "^has_journal"
            ];
          }
        ];

        writableStoreOverlay = "/nix/.rw-store";
      };

      networking.nameservers = [ hostAddress ];

      systemd.network.networks = {
        "10-lan" = {
          matchConfig.MACAddress = mac;
          address = [ "${guestAddress}/24" ];
          routes = [ { Gateway = hostAddress; } ];
          dns = [ hostAddress ];
        };
        "20-container" = {
          matchConfig.Name = [
            "veth*"
            "podman*"
          ];
          linkConfig.Unmanaged = true;
        };
      };

      # Host keys live on the persistent volume; /etc is tmpfs here, so the
      # default location would mint new keys on every restart.
      services.openssh.hostKeys = [
        {
          path = "/var/lib/ssh/ssh_host_ed25519_key";
          type = "ed25519";
        }
      ];

      systemd.tmpfiles.rules = [ "d /var/lib/ssh 0755 root root - -" ];
      users.users.root.openssh.authorizedKeys.keys = sshKeys;
      networking.firewall = {
        allowedTCPPorts = [ 22 ];
        # Job containers reach the cache proxy (cache.proxy_port) over the
        # podman bridge, so open just that pinned port.
        interfaces."podman0".allowedTCPPorts = [ proxyPort ];
      };

      virtualisation.podman = {
        enable = true;
        dockerSocket.enable = true;
      };

      services.forgejo-runner = {
        instances.runner = {
          enable = true;

          settings = {
            runner = {
              capacity = 4;

              labels = [
                "docker:docker://localhost/forgejo-runner:latest"
                "native:host"
              ];

              envs = {
                # Added to remove the need to strip the protocol (https://)
                # from FORGEJO_SERVER_URL when cloning private repositories:
                #   git clone --depth=1 --branch "${{ forgejo.ref_name }}" \
                #     "https://x-access-token:${{ secrets.FORGEJO_TOKEN }}@$FORGEJO_DOMAIN/${{ forgejo.repository }}" .
                FORGEJO_DOMAIN = forgejoDomain;
              };
            };

            server.connections.default = {
              url = forgejoUrl;
              uuid = "5ff7fea6-1a54-4abf-a058-6b9ffa5ba5f5";
            };

            cache = {
              enabled = true;
              host = "host.containers.internal";
              port = cachePort;
              proxy_port = proxyPort;
            };

            container = {
              network = "podman";
              force_pull = false;

              options = lib.concatMapStringsSep " " (v: "-v ${v.host}:${v.guest}") containerVolumes;
              valid_volumes = map (v: v.host) containerVolumes;
            };
          };

          secrets.server.connections.default.token_url = "${secretsMount}/token";

          # PATH for native:host jobs (host-mode runs with only these on PATH).
          hostPackages = with pkgs; [
            bash
            coreutils
            curl
            gawk
            git
            gnused
            gnutar
            gzip
            nodejs
            wget
            config.nix.package
          ];
        };
      };

      # Load the Nix-built runner image into podman's (rootful) local storage
      # before the runner starts, and refresh it on every activation. The runner
      # talks to the rootful podman socket, so this must run as root too.
      systemd.services.forgejo-runner-image-load = {
        description = "Load the Nix-built Forgejo runner image into podman";
        wantedBy = [ "multi-user.target" ];
        before = [ "forgejo-runner-runner.service" ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          ExecStart = "${config.virtualisation.podman.package}/bin/podman load --input ${image}";
        };
      };

      nix.settings.experimental-features = [
        "nix-command"
        "flakes"
      ];

      # The root filesystem is tmpfs, so builds would otherwise be capped by RAM.
      systemd.services.nix-daemon.environment.TMPDIR = "/var/tmp";

      time.timeZone = hostTimeZone;
      system.stateVersion = "26.05";
    };
}
