{ config, lib, pkgs, ... }:

with pkgs.lib;
{
  options.meta = {
    username = mkOption {
      type = types.str;
      description = "The default username";
      default = "victor";
    };
  };
}
