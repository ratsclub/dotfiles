{
  services.openssh = {
    enable = true;
    settings.PasswordAuthentication = false;
    # Automatically remove stale sockets
    settings.StreamLocalBindUnlink = "yes";
    # Allow forwarding ports to everywhere
    settings.GatewayPorts = "clientspecified";
  };
}
