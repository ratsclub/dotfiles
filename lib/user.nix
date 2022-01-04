{
  users.mutableUsers = false;

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
