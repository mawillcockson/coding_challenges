{
  # receives(*) pkgs.hello and pkgs.writeShellApplication
  hello,
  writeShellApplication,
  # can be overridden with `yourPackage.override { traditionalGreeting = true; }`
  traditionalGreeting ? false,
  greeting ? null,
}:
writeShellApplication {
  name = "hello-wrapper";
  runtimeInputs = [
    hello
  ];
  derivationArgs = {inherit greeting;};
  text = ''
    hello${
      if traditionalGreeting
      then " -t"
      else ""
    }${
      if builtins.isNull greeting || greeting == ""
      then ""
      else '' -g "$greeting"''
    }
  '';
}
