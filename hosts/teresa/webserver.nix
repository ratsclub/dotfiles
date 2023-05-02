{ config, lib, pkgs, ... }:

{
  services.caddy = {
    enable = true;
    virtualHosts = {
      "glorifiedgluer.com".extraConfig = ''
        redir https://gluer.org{uri}
      '';

      "media.glorifiedgluer.com".extraConfig = ''
        redir https://media.gluer.org{uri}
      '';

      "media.gluer.org".extraConfig = ''
        reverse_proxy olga:8096
      '';
    };
  };
}
