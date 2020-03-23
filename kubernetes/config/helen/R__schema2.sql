INSERT into entity(row_key, column_name, version, body, user_id, user_name) VALUES ('19a63658-0878-4ec2-a165-081b9e1820ad', 'helen.zone', 1, '{
  "name": "US East (N. Virginia)",
  "latitude": "45.2551139",
  "longitude": "-120.888727",
  "type": "VMC_AWS",
  "csp_url": "https://console.cloud.vmware.com",
  "vmc_url": "https://vmc.vmware.com",
  "refresh_token": "{{APITOKEN}}",
  "organization": "c56e116e-c36f-4f7d-b504-f9a33955b853",
  "datacenter": "3656526b-c74e-4f87-8e1f-a667975273c2",
  "resource_pool": "Compute-ResourcePool",
  "storage": "WorkloadDatastore",
  "folder": "{{VMFolder}}",
  "network": {
    "name": "blockchain-control",
    "gateway": "10.2.0.1",
    "subnet": "16"
  },
  "wavefront": {
    "url": "https://vmware.wavefront.com",
    "token": "{{WAVEFRONT_TOKEN}}"
  },
  "log_managements": [
    {
      "destination": "LOG_INTELLIGENCE",
      "address": "https://data.mgmt.cloud.vmware.com/le-mans/v1/streams/ingestion-pipeline-stream",
      "token": "{{LINT_TOKEN}}"
    }
  ]
}'::JSONB, '51123b25-d017-4afa-8a1c-4e99badb24c6', 'khank@vmware.com') ON CONFLICT (row_key,version) DO UPDATE SET body = excluded.body;


INSERT into entity(row_key, column_name, version, body, user_id, user_name) VALUES ('7dd33511-f847-4137-858b-1499558cfea7', 'helen.zone', 1, '{
  "name": "EU Central (Frankfurt)",
  "latitude": "52.270050",
  "longitude": "10.394489",
  "type": "VMC_AWS",
  "csp_url": "https://console.cloud.vmware.com",
  "vmc_url": "https://vmc.vmware.com",
  "refresh_token": "{{APITOKEN}}",
  "organization": "c56e116e-c36f-4f7d-b504-f9a33955b853",
  "datacenter": "6db19f8f-cde6-4151-88e5-a3b0d6aead6a",
  "resource_pool": "Compute-ResourcePool",
  "storage": "WorkloadDatastore",
  "folder": {{APITOKEN}}",
  "network": {
    "name": "blockchain-control",
    "gateway": "10.3.0.1",
    "subnet": "16"
  },
  "wavefront": {
    "url": "https://vmware.wavefront.com",
    "token": "{{WAVEFRONT_TOKEN}}"
  },
  "log_managements": [
    {
      "destination": "LOG_INTELLIGENCE",
      "address": "https://data.mgmt.cloud.vmware.com/le-mans/v1/streams/ingestion-pipeline-stream",
      "token": "{{LINT_TOKEN}}"
    }
  ]
}'::JSONB, '51123b25-d017-4afa-8a1c-4e99badb24c6', 'khank@vmware.com') ON CONFLICT (row_key,version) DO UPDATE SET body = excluded.body;
