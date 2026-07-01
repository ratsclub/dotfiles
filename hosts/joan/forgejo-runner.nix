{
  config,
  pkgs,
  inputs,
  ...
}:

let
  forgejoUrl =
    inputs.self.nixosConfigurations.catarina.config.services.forgejo.settings.server.ROOT_URL;
in
{
  age.secrets.forgejo-runner-token.file = ../../secrets/joan/forgejo/runner-token.age;

  virtualisation.podman = {
    enable = true;
    dockerSocket.enable = true;
  };

  # Cache server port, reachable only from the podman bridge.
  networking.firewall.interfaces."podman0".allowedTCPPorts = [ 42000 ];

  services.forgejo-runner = {
    instances.joan = {
      enable = true;

      settings = {
        runner.labels = [
          "docker:docker://node:24-bookworm"
          "ubuntu-latest:docker://node:24-bookworm"
          "native:host"
        ];

        server.connections.default = {
          url = forgejoUrl;
          uuid = "27536b59-cbfd-4c07-a1cd-7cc416c81b36";
        };

        # Built-in cache server, advertised to jobs via podman's host alias.
        cache = {
          enabled = true;
          host = "host.containers.internal";
          port = 42000;
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
