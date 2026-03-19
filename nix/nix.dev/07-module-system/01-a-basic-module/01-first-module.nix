{ lib, ... }:
{
  options.name = lib.mkOption { type = lib.types.str; };
  config.name = "A.U. Thor";
}
