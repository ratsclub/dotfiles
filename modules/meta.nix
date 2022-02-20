{ config, lib, pkgs, ... }:

with pkgs.lib;
{
  options.meta = {
    username = mkOption {
      type = types.str;
      description = "The default username";
      default = "victor";
    };

    email = mkOption {
      type = types.str;
      description = "The default email";
      default = "victor@freire.dev.br";
    };

    name = mkOption {
      type = types.str;
      description = "The default name";
      default = "Victor Freire";
    };
  };
}
