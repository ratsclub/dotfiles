{
  config,
  pkgs,
  inputs,
  ...
}:

let
  forgejoUrl =
    inputs.self.nixosConfigurations.catarina.config.services.forgejo.settings.server.ROOT_URL;
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
            "docker:docker://node:24-bookworm"
            "ubuntu-latest:docker://node:24-bookworm"
            "native:host"
          ];
        };

        server.connections.default = {
          url = forgejoUrl;
          uuid = "27536b59-cbfd-4c07-a1cd-7cc416c81b36";
        };

        # Built-in cache server, advertised to jobs via podman's host alias.
        # Jobs connect to proxy_port; port is the host-side backend. Both are
        # pinned so the firewall rule above can target a fixed port.
        cache = {
          enabled = true;
          host = "host.containers.internal";
          port = 42000;
          proxy_port = proxyPort;
        };

        # Pin jobs to the default bridge so the gateway/interface stay stable.
        container.network = "podman";
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
}
