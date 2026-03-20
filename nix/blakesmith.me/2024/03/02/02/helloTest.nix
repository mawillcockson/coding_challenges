{
  self,
  pkgs,
  ...
}:
pkgs.testers.runNixOSTest {
  name = "hello-boots";
  nodes.machine = {...}: {
    imports = [
      self.nixosModules.hello
    ];
    services.helloNixosTests.enable = true;

    system.stateVersion = "25.11";
  };

  testScript = ''
    machine.wait_for_unit("helloNixosTests.service")
    machine.wait_for_open_port(3000)
  '';
}
