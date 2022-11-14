{ config, inputs, lib, pkgs, ... }:

let
  grafanaDomain = "grafana.glorifiedgluer.com";
in
{
  services = {
    caddy.enable = lib.mkDefault true;

    caddy.virtualHosts."${grafanaDomain}" = {
      extraConfig = ''
        reverse_proxy 127.0.0.1:${toString config.services.grafana.settings.server.http_port}
      '';
    };
    grafana = {
      enable = true;
      settings = {
        server.domain = grafanaDomain;
        server.http_port = 10010;
      };

      provision.dashboards.settings.providers = [
        {
          name = "Node Full";
          options.path = ./dashboards/node-exporter-full.json;
        }
        {
          name = "ZFS Pool metrics";
          options.path = ./dashboards/zfs-pool-metrics.json;
        }
      ];

      provision.datasources.settings.datasources =
        [
          {
            name = "Prometheus";
            type = "prometheus";
            url = "http://localhost:${toString config.services.prometheus.port}";
          }
          {
            name = "Loki";
            type = "loki";
            url = "http://localhost:${toString config.services.loki.configuration.server.http_listen_port}";
          }
        ];
    };

    prometheus = {
      enable = true;
      port = 10020;

      exporters = {
        node = {
          enable = true;
          enabledCollectors = [ "systemd" "processes" ];
          port = 3021;
        };
      };

      scrapeConfigs = [
        {
          job_name = "teresa";
          static_configs = [{
            targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.node.port}" ];
          }];
        }
      ];
    };

    loki = {
      enable = true;
      configuration = {
        auth_enabled = false;
        server.http_listen_port = 10030;

        ingester = {
          lifecycler = {
            address = "127.0.0.1";
            ring = {
              kvstore = {
                store = "inmemory";
              };
              replication_factor = 1;
            };
          };
          chunk_idle_period = "1h";
          max_chunk_age = "1h";
          chunk_target_size = 999999;
          chunk_retain_period = "30s";
          max_transfer_retries = 0;
        };

        schema_config = {
          configs = [{
            from = "2022-06-06";
            store = "boltdb-shipper";
            object_store = "filesystem";
            schema = "v11";
            index = {
              prefix = "index_";
              period = "24h";
            };
          }];
        };

        storage_config = {
          boltdb_shipper = {
            active_index_directory = "/var/lib/loki/boltdb-shipper-active";
            cache_location = "/var/lib/loki/boltdb-shipper-cache";
            cache_ttl = "24h";
            shared_store = "filesystem";
          };

          filesystem = {
            directory = "/var/lib/loki/chunks";
          };
        };

        limits_config = {
          reject_old_samples = true;
          reject_old_samples_max_age = "168h";
        };

        chunk_store_config = {
          max_look_back_period = "0s";
        };

        table_manager = {
          retention_deletes_enabled = false;
          retention_period = "0s";
        };

        compactor = {
          working_directory = "/var/lib/loki";
          shared_store = "filesystem";
          compactor_ring = {
            kvstore = {
              store = "inmemory";
            };
          };
        };
      };
    };
  };

  services.promtail = {
    enable = true;
    configuration = {
      server = {
        http_listen_port = 10040;
        grpc_listen_port = 0;
      };
      positions.filename = "/tmp/positions.yaml";
      clients = [{
        url = "http://127.0.0.1:${toString config.services.loki.configuration.server.http_listen_port}/loki/api/v1/push";
      }];
      scrape_configs = [{
        job_name = "journal";
        journal = {
          max_age = "12h";
          labels = {
            job = "systemd-journal";
            host = "teresa";
          };
        };
        relabel_configs = [{
          source_labels = [ "__journal__systemd_unit" ];
          target_label = "unit";
        }];
      }];
    };
  };
}
