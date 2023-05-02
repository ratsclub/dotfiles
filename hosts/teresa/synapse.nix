{ config, lib, pkgs, ... }:

let
  domain = config.networking.domain;
  matrixDomain = "matrix.${domain}";
in
{
  services.caddy.virtualHosts = {
    "${domain}" =
      let
        # use 443 instead of the default 8008 port to unite
        # the client-server and server-server port for simplicity
        server = { "m.server" = "${matrixDomain}:443"; };
        client = {
          "m.homeserver" = { "base_url" = "https://${matrixDomain}"; };
        };
      in
      {
        extraConfig = ''
          header /.well-known/matrix/* Content-Type application/json
     	    header /.well-known/matrix/* Access-Control-Allow-Origin *
     	    respond /.well-known/matrix/server `${builtins.toJSON server}`
     	    respond /.well-known/matrix/client `${builtins.toJSON client}`
        '';
      };

    "${matrixDomain}" = {
      extraConfig = ''
        reverse_proxy /_matrix/* olga:8008
        reverse_proxy /_synapse/client/* olga:8008
      '';
    };
  };
}
