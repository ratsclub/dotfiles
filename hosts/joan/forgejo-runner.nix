{
  config,
  pkgs,
  inputs,
  ...
}:

let
  forgejoUrl =
    inputs.self.nixosConfigurations.catarina.config.services.forgejo.settings.server.ROOT_URL;
  forgejoDomain =
    inputs.self.nixosConfigurations.catarina.config.services.forgejo.settings.server.DOMAIN;
  proxyPort = 42001;
in
{
  age.secrets.forgejo-runner-token.file = ../../secrets/joan/forgejo/runner-token.age;

  virtualisation.podman = {
    enable = true;
    dockerSocket.enable = true;
  };

  # Job containers reach the cache proxy (cache.proxy_port) over the podman
  # bridge, so open just that pinned port.
  networking.firewall.interfaces."podman0".allowedTCPPorts = [ proxyPort ];

  services.forgejo-runner = {
    instances.joan = {
      enable = true;

      settings = {
        runner = {
          # Number of jobs to run in parallel.
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
          uuid = "27536b59-cbfd-4c07-a1cd-7cc416c81b36";
        };

        cache = {
          enabled = true;
          host = "host.containers.internal";
          port = 42000;
          proxy_port = proxyPort;
        };

        container = {
          network = "podman";
          force_pull = false;
        };
      };

      secrets.server.connections.default.token_url = config.age.secrets.forgejo-runner-token.path;

      # PATH for native:host jobs (host-mode runs with only these on PATH).
      hostPackages = with pkgs; [
        bash
        coreutils
        curl
        gawk
        gitMinimal
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
    before = [ "forgejo-runner-joan.service" ];
    after = [ "network.target" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = "${config.virtualisation.podman.package}/bin/podman load --input ${pkgs.forgejo-runner-image}";
    };
  };
}
