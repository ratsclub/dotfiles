let
  users = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHcM1qHIUVmhRC0jY8Tzvu6SdTn+68cM7ArPw3AwD/LN"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIImly9yO1lUBeqsAgWYDHOYj8hYUg/zyvGb5X/qRsMNB"

    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIey4LP6XkLhU8kBxCu0zW+LriyMu0xFyuftv29fkxKS root@capivaras"
  ];
in
{
  "forgejomail.age".publicKeys = users;
  "appriseconfig.age".publicKeys = users;
}
