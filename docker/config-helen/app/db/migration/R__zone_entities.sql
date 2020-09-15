INSERT into entity(row_key, column_name, version, body, user_id, user_name) VALUES ('7eef6110-68bc-11ea-906e-8c859085f3e7', 'helen.zone', 1, '{
  "name": "VMC SDDC4",
  "latitude": "45.2551139",
  "longitude": "-120.888727",
  "type": "VMC_AWS",
  "csp_url": "https://console.cloud.vmware.com",
  "vmc_url": "https://vmc.vmware.com",
  "refresh_token": "<VMC_API_TOKEN>",
  "organization": "c56e116e-c36f-4f7d-b504-f9a33955b853",
  "datacenter": "8128e6b3-8a87-430a-85a1-386cefae66e3",
  "resource_pool": "Compute-ResourcePool",
  "storage": "WorkloadDatastore",
  "folder": "HelenTesting",
  "network": {
    "name": "sddc6-vmware-vpn-cloud-5",
    "gateway": "10.72.217.1",
    "subnet": 24,
    "name_servers": ["10.78.1.132", "10.78.1.133"]
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
}'::JSONB, '51123b25-d017-4afa-8a1c-4e99badb24c6', 'svc.blockchain_1@vmware.com') ON CONFLICT (row_key,version) DO UPDATE SET body = excluded.body;
