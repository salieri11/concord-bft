INSERT into entity(row_key, column_name, version, body, user_id, user_name) VALUES ('155de65a-da76-11ea-87d0-0242ac130003', 'helen.nodesizetemplate', 1, '{
  "name": "Node size template",
  "templates": 
  [
    {
      "name": "Small",
      "items":
      [
        {
          "type": "committer",
          "no_of_cpus": "4",
          "storage_in_gigs": "1024",
          "memory_in_gigs": "32"
        },
        {
          "type": "client",
          "no_of_cpus": "4",
          "storage_in_gigs": "1024",
          "memory_in_gigs": "32"
        }
      ]
    },
    {
      "name": "Medium",
      "items": 
      [
        {
          "type": "committer",
          "no_of_cpus": "8",
          "storage_in_gigs": "1024",
          "memory_in_gigs": "32"
        },
        {
          "type": "client",
          "no_of_cpus": "8",
          "storage_in_gigs": "1024",
          "memory_in_gigs": "32"
        }
      ]
    },
    {
      "name": "Large",
      "items": 
      [
        {
          "type": "committer",
          "no_of_cpus": "1",
          "storage_in_gigs": "1024",
          "memory_in_gigs": "64"
        },
        {
          "type": "committer",
          "no_of_cpus": "1",
          "storage_in_gigs": "1024",
          "memory_in_gigs": "64"
        }
      ]
    }
  ],
  "range": 
  {
    "no_of_cpus": 
    {
      "min": 1,
      "max": 18
    },
    "storage_in_gigs": 
    {
      "min": 1,
      "max": 16384
    },
    "memory_in_gigs": 
    {
      "min": 1,
      "max": 3024
    }
  }
}'::JSONB, '51123b25-d017-4afa-8a1c-4e99badb24c6', 'svc.blockchain_1@vmware.com') ON CONFLICT (row_key,version) DO UPDATE SET body = excluded.body;
