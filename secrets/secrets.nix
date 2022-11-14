let
  users = [
    # victor
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIEpBZXIOn5Eeq2peV4gH3jSf2fqinRnTPHd1NHlscLZ"

    # teresa
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIB3vvKpUUTOGnxG+PmrkxrwU0OGG8qZ1LBqmkjxYGenI"
  ];
in
{
  "mailbox.age".publicKeys = users;
  "wireguard.age".publicKeys = users;
  "transmission.age".publicKeys = users;
}
