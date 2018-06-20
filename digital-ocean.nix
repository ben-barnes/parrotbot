{
  resources.sshKeyPairs.ssh-key = {};

  webserver = { config, pkgs, ... }: {
    services.openssh.enable = true;
    services.nginx = {
      enable = true;
      virtualHosts = {
        "expression-oriented.net" = {
          forceSSL = true;
          enableACME = true;
          locations."/" = {
            root = "/var/www";
          };
        };
      };
    };

    networking.firewall.allowedTCPPorts = [ 80 443 ];

    deployment.targetEnv = "digitalOcean";
    deployment.digitalOcean.enableIpv6 = true;
    deployment.digitalOcean.region = "sgp1";
    deployment.digitalOcean.size = "512mb";
  };
}
