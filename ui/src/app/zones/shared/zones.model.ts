/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

export interface Zone {
  type?: ZoneType;
  id?: string;
  name?: string;
  latitude?: number;
  longitude?: number;
  password?: string;
}

export interface OnPremZone extends Zone {
  folder: string;
  resource_pool: string;
  storage: string;
  vcenter: OnPremVCenter;
  container_repo: OnPremContainerRepo;
  network: OnPremNetwork;
  log_managements: LogManagements[];
}

export interface OnPremVCenter {
  password: string;
  url: string;
  username: string;
}

export interface OnPremContainerRepo {
  password: string;
  url: string;
  username: string;
}

export interface OnPremNetwork {
  gateway: string;
  name_servers?: [];
  ip_pool: [];
  name: string;
  subnet: string;
}

export interface LogManagements {
  address: string;
  port: number;
  username: string;
  password: string;
}

export enum ZoneType {
  NONE = 'NONE',
  VMC_AWS = 'VMC_AWS',
  ON_PREM = 'ON_PREM'
}

export const fakeZones: Zone[] = [{
  name: 'US West - Oregon',
  id: 'us-west',
  latitude: 0,
  longitude: 0,
  type: ZoneType.VMC_AWS
}, {
  name: 'US East - N Virginia',
  id: 'us-east',
  latitude: 0,
  longitude: 0,
  type: ZoneType.VMC_AWS
}, {
  name: 'EMEA - Frankfurt',
  id: 'emea',
  latitude: 0,
  longitude: 0,
  type: ZoneType.VMC_AWS
},
{
  name: 'Pacific - Sydney',
  id: 'pacific',
  latitude: 0,
  longitude: 0,
  type: ZoneType.VMC_AWS
}];
