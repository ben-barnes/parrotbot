let parrotbot = (import ./parrotbot { }).parrotbot;
in {
  resources.sshKeyPairs.ssh-key = {};

  webserver = { config, pkgs, ... }: {
    # OpenSSH service
    services.openssh.enable = true;

    # NGINX service
    services.nginx = {
      enable = true;
      virtualHosts = {
        "parrotbot.expression-oriented.net" = {
          forceSSL = true;
          enableACME = true;
          locations."/" = {
            proxyPass = http://localhost:8080;
          };
        };
      };
    };

    # Parrotbot service
    systemd.services.parrotbot = {
      description = "Parrotbot Daemon";
      serviceConfig = {
        Type = "simple";
        ExecStart = "${parrotbot}/bin/parrotbot --app-token /opt/parrotbot/slack-app --bot-token /opt/parrotbot/slack-bot";
        ExecStop  = "/run/current-system/sw/bin/pkill parrotbot";
        Restart = "on-failure";
      };
      wantedBy = [ "default.target" ];
    };

    systemd.services.parrotbot.enable = true;

    networking.firewall.allowedTCPPorts = [ 80 443 ];

    deployment.targetEnv = "digitalOcean";
    deployment.digitalOcean.enableIpv6 = true;
    deployment.digitalOcean.region = "sgp1";
    deployment.digitalOcean.size = "1gb";

    # Upload and persist Slack API tokens from local files.
    deployment.keys."slack-app".text = builtins.readFile ./credentials/slack-app.token;
    deployment.keys."slack-bot".text = builtins.readFile ./credentials/slack-bot.token;
    systemd.services.nixops-keys.postStart =  ''
      mkdir -p /opt/parrotbot/
      cp /run/keys/slack-app /opt/parrotbot/
      cp /run/keys/slack-bot /opt/parrotbot/
    '';
  };
}
