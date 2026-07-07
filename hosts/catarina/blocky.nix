{ config, ... }:

let
  cfg = config.services.blocky;
in
{
  networking.firewall.allowedUDPPorts = [ cfg.settings.ports.dns ];
  networking.firewall.allowedTCPPorts = [ cfg.settings.ports.dns ];

  services.blocky = {
    enable = true;
    settings = {
      ports = {
        dns = 53;
        # Prometheus metrics
        http = 4000;
      };

      upstreams = {
        groups.default = [
          "https://one.one.one.one/dns-query"
          "https://dns.quad9.net/dns-query"
        ];
        strategy = "parallel_best";
      };

      bootstrapDns = [
        { upstream = "1.1.1.1"; }
        { upstream = "9.9.9.9"; }
      ];

      blocking = {
        denylists.ads = [
          "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts"
          "https://cdn.jsdelivr.net/gh/hagezi/dns-blocklists@latest/wildcard/pro.txt"
        ];
        clientGroupsBlock.default = [ "ads" ];
        # Answer blocked names with 0.0.0.0 / :: rather than NXDOMAIN.
        blockType = "zeroIp";
        loading.refreshPeriod = "24h";
      };

      caching = {
        minTime = "5m";
        prefetching = true;
      };

      prometheus.enable = true;
    };
  };
}
