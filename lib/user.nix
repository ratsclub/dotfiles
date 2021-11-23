{
  users.users.victor = {
    isNormalUser = true;
    extraGroups = [
      "docker"
      "kvm"
      "libvirtd"
      "networkmanager"
      "wheel"
    ];
    password = "changeme";
  };
}
