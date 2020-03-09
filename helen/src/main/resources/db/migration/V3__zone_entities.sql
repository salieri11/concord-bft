INSERT into entity(row_key, column_name, version, body, user_id, user_name) VALUES ('6adaf48a-9075-4e35-9a71-4ef1fb4ac90f', 'helen.zone', 1, '{
  "name": "VMC SDDC4",
  "latitude": "45.2551139",
  "longitude": "-120.888727",
  "type": "VMC_AWS",
  "csp_url": "https://console.cloud.vmware.com",
  "vmc_url": "https://vmc.vmware.com",
  "refresh_token": "pQ1crf7lC1cTP0rj3tPIN838qHUcuXTCnsEt45URjdyULvSZ4nQ7CkwBS46OTLd9",
  "organization": "c56e116e-c36f-4f7d-b504-f9a33955b853",
  "datacenter": "abb0fe0e-1f2f-470c-a427-774aa08ce2ea",
  "resource_pool": "Compute-ResourcePool",
  "storage": "WorkloadDatastore",
  "folder": "HermesTesting",
  "network": {
    "name": "vmware-vpn",
    "gateway": "10.69.105.1",
    "subnet": "24"
  },
  "wavefront": {
    "url": "https://vmware.wavefront.com",
    "token": "<WAVEFRONT_API_TOKEN>"
  },
  "log_managements": [
    {
      "destination": "LOG_INTELLIGENCE",
      "address": "https://data.mgmt.cloud.vmware.com/le-mans/v1/streams/ingestion-pipeline-stream",
      "token": "<FLUENTD_AUTHORIZATION_BEARER>"
    }
  ]
}'::JSONB, '51123b25-d017-4afa-8a1c-4e99badb24c6', 'khank@vmware.com') ON CONFLICT (row_key,version) DO UPDATE SET body = excluded.body;


INSERT into entity(row_key, column_name, version, body, user_id, user_name) VALUES ('623e6f81-5954-4b32-9cdb-eb5d8dd913db', 'helen.zone', 1, '{
  "name": "VMC SDDC3",
  "latitude": "39.028035",
  "longitude": "-77.457941",
  "type": "VMC_AWS",
  "csp_url": "https://console.cloud.vmware.com",
  "vmc_url": "https://vmc.vmware.com",
  "refresh_token": "pQ1crf7lC1cTP0rj3tPIN838qHUcuXTCnsEt45URjdyULvSZ4nQ7CkwBS46OTLd9",
  "organization": "c56e116e-c36f-4f7d-b504-f9a33955b853",
  "datacenter": "6db19f8f-cde6-4151-88e5-a3b0d6aead6a",
  "resource_pool": "Compute-ResourcePool",
  "storage": "WorkloadDatastore",
  "folder": "HermesTesting",
  "network": {
    "name": "vmware-vpn",
    "gateway": "10.69.100.1",
    "subnet": "24"
  },
  "wavefront": {
    "url": "https://vmware.wavefront.com",
    "token": "<WAVEFRONT_API_TOKEN>"
  },
  "log_managements": [
    {
      "destination": "LOG_INTELLIGENCE",
      "address": "https://data.mgmt.cloud.vmware.com/le-mans/v1/streams/ingestion-pipeline-stream",
      "token": "<FLUENTD_AUTHORIZATION_BEARER>"
    }
  ]
}'::JSONB, '51123b25-d017-4afa-8a1c-4e99badb24c6', 'khank@vmware.com') ON CONFLICT (row_key,version) DO UPDATE SET body = excluded.body;
