{
  users.mutableUsers = true;

  users.users.victor = {
    isNormalUser = true;
    createHome = true;
    extraGroups = [
      "docker"
      "kvm"
      "libvirtd"
      "networkmanager"
      "wheel"
    ];
    initialPassword = "changeme";
  };
}
