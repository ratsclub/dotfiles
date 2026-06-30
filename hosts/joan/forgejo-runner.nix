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
  # Registration token, as a systemd EnvironmentFile (contents: TOKEN=<token>).
  age.secrets.forgejo-runner-token.file = ../../secrets/joan/forgejo/runner-token.age;

  # docker-compat socket backs the runner's docker:// labels.
  virtualisation.podman = {
    enable = true;
    dockerSocket.enable = true;
  };

  # Cache server port, reachable only from the podman bridge.
  networking.firewall.interfaces."podman0".allowedTCPPorts = [ 42000 ];

  services.gitea-actions-runner = {
    package = pkgs.forgejo-runner;
    instances.joan = {
      enable = true;
      name = config.networking.hostName;
      url = forgejoUrl;
      tokenFile = config.age.secrets.forgejo-runner-token.path;
      settings = {
        # Built-in cache server, advertised to jobs via podman's host alias.
        cache = {
          enabled = true;
          host = "host.containers.internal";
          port = 42000;
        };
        # Pin jobs to the default bridge so the gateway/interface stay stable.
        container.network = "podman";
      };
      # docker:// runs in podman; native:host runs on the host (needs the Nix daemon).
      labels = [
        "docker:docker://node:24-bookworm"
        "ubuntu-latest:docker://node:24-bookworm"
        "native:host"
      ];
      # PATH for nix:host jobs (host-mode runs with only these on PATH).
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
