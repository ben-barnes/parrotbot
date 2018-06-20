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
      enable = true;
      serviceConfig = {
        Type = "forking";
        ExecStart = ''
          ${parrotbot}/bin/parrotbot\
            --app-token $(cat /opt/parrotbot/slack-app.token)\
            --bot-token $(cat /opt/parrotbot/slack-bot.token)
          '';
        ExecStop  = "pkill parrotbot";
        Restart = "on-failure";
      };
      after =  [ "nixops-keys.service" ];
      wantedBy = [ "default.target" ];
    };

    networking.firewall.allowedTCPPorts = [ 80 443 ];

    deployment.targetEnv = "digitalOcean";
    deployment.digitalOcean.enableIpv6 = true;
    deployment.digitalOcean.region = "sgp1";
    deployment.digitalOcean.size = "1gb";

    # Upload and persist Slack API token from local file.
    deployment.keys."slack-app.token".text = builtins.readFile ./credentials/slack-app.token;
    deployment.keys."slack-bot.token".text = builtins.readFile ./credentials/slack-bot.token;
    systemd.services.nixops-keys.postStart = ''
      mkdir -p /opt/parrotbot
      cp /run/keys/slack-app.token /opt/parrotbot/
      cp /run/keys/slack-bot.token /opt/parrotbot/
    '';
  };
}
