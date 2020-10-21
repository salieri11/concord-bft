INSERT into entity(row_key, column_name, version, body, user_id, user_name) VALUES ('155de65a-da76-11ea-87d0-0242ac130003', 'helen.nodesizetemplate', 1, '{
  "name": "Node size template",
  "templates": 
  [
    {
      "name": "Small",
      "items":
      [
        {
          "type": "replica",
          "no_of_cpus": "2",
          "storage_in_gigs": "64",
          "memory_in_gigs": "16"
        },
        {
          "type": "client",
          "no_of_cpus": "2",
          "storage_in_gigs": "64",
          "memory_in_gigs": "16"
        }
      ]
    },
    {
      "name": "Medium",
      "items": 
      [
        {
          "type": "replica",
          "no_of_cpus": "16",
          "storage_in_gigs": "256",
          "memory_in_gigs": "64"
        },
        {
          "type": "client",
          "no_of_cpus": "16",
          "storage_in_gigs": "256",
          "memory_in_gigs": "64"
        }
      ]
    },
    {
      "name": "Large",
      "items": 
      [
        {
          "type": "replica",
          "no_of_cpus": "32",
          "storage_in_gigs": "1024",
          "memory_in_gigs": "128"
        },
        {
          "type": "client",
          "no_of_cpus": "32",
          "storage_in_gigs": "1024",
          "memory_in_gigs": "128"
        }
      ]
    }
  ],
  "range": 
  {
    "no_of_cpus": 
    {
      "min": 2,
      "max": 32
    },
    "storage_in_gigs": 
    {
      "min": 64,
      "max": 4096 
    },
    "memory_in_gigs": 
    {
      "min": 16,
      "max": 128
    }
  }
}'::JSONB, '51123b25-d017-4afa-8a1c-4e99badb24c6', 'svc.blockchain_1@vmware.com') ON CONFLICT (row_key,version) DO UPDATE SET body = excluded.body;
