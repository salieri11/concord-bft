/*
 * Copyright 2018-2020 VMware, all rights reserved.
 */
import { Component, OnInit } from '@angular/core';
import { BlockchainService } from '../../blockchain/shared/blockchain.service';
import { OrgService } from '../../orgs/shared/org.service';
import { ContractEngines } from '../../blockchain/shared/blockchain.model';
import { Zone, ZoneType } from '../../zones/shared/zones.model';
import { BlockchainNode, NodeCredentials } from '../../nodes/shared/nodes.model';
import { Personas } from '../../shared/persona.service';


class SystemInfo {
  organization_id: string;
  blockchain_id: string;
  product_version: string;
  engine_version: string;
  engine_type: string;
  created_by: string;
  created_date: string;
}

@Component({
  selector: 'concord-system',
  templateUrl: './system.component.html',
  styleUrls: ['./system.component.scss']
})
export class SystemComponent implements OnInit {

  personas = Personas;

  selectedTab = '';
  zones: { [id: string]: Zone } = {};
  zoneFirst: Zone = null;
  blockchainType: ContractEngines;

  securePasswordEnabled: boolean = false;
  securePasswordModalShown: boolean = false;
  securePasswordFetching: boolean = false;
  securePasswordHidden: boolean = true;
  securePasswordNode: BlockchainNode;
  securePasswordData: NodeCredentials;
  securePasswordCached: { [nodeId: string]: NodeCredentials } = {};

  // Blockchain Info
  orgId = '';
  // TODO: get this info from actual API
  productVersion = '';
  createdBy = '';
  createdDate = 0;
  createdDateLocalTimezone = '';
  createdDateISO = '';
  execEngineName = 'DAML Ledger';
  execEngineVersion = '_';
  execEngineVersionSnapshot = '';
  execEngineVersionSnapshot2 = '';
  execEngineVersionFull = '';
  concordName = 'Concord';
  concordVersion = 'Concord BFT';
  systemInfo: SystemInfo = new SystemInfo();

  get systemInfoJSON(): string {
    return JSON.stringify(this.systemInfo);
  }


  ready: boolean = false;
  fetchedList = { organizations: false, nodes: false, zones: false };



  constructor(
    public blockchainService: BlockchainService,
    private orgService: OrgService,
  ) {
    this.orgService.getList().subscribe(resp => {
      this.orgId = resp[0].organization_id;
      this.systemInfo.organization_id = this.orgId;
    });
    this.fetchedList.nodes = true;
    this.updateBaseOnSelectedBlockchain();
  }

  ngOnInit() {
    // Nudges UI view to update about selected bc (even if it looks it's doing nothing)
    this.updateBaseOnSelectedBlockchain();
    return this.blockchainService.selectedBlockchain;
  }

  getZoneInfo(zoneId: string) {
    if (!this.zones[zoneId]) { return null; }
    const zone = this.zones[zoneId];
    const zoneType = zone.type === ZoneType.VMC_AWS ? 'Cloud' : 'On-premises';
    return zone.name + ' (' + zoneType + ')';
  }

  async updateBaseOnSelectedBlockchain() {
    const blockchain = this.blockchainService.selectedBlockchain;
    this.blockchainType = blockchain.blockchain_type as ContractEngines;
    if (this.blockchainType === ContractEngines.DAML) {
      this.execEngineName = 'DAML Ledger';
    } else if (this.blockchainType === ContractEngines.ETH) {
      this.execEngineName = 'Ethereum';
      this.execEngineVersion = 'EVM';
    }
    const versionsList = blockchain.version ? blockchain.version.split(',') : '';
    for (const versionString of versionsList) {
      if (versionString.indexOf('Blockchain Version: ') >= 0) {
        this.productVersion = versionString.split('Blockchain Version: ')[1];
      }
      if (versionString.indexOf('DAML SDK Version: ') >= 0) {
        this.execEngineVersion = 'SDK v' + versionString.split('DAML SDK Version: ')[1];
        this.execEngineVersionFull = versionString.split('DAML SDK Version: ')[1];
      }
    }
    // Handle long version with snapshot info
    if (this.execEngineVersion.indexOf('-snapshot.') >= 0) {
      const longDAMLVersionSplit = this.execEngineVersion.split('-snapshot.');
      this.execEngineVersion = longDAMLVersionSplit[0] + ' ';
      const snapShotSplit = longDAMLVersionSplit[1].split('.');
      this.execEngineVersionSnapshot = snapShotSplit[0] + ' ';
      snapShotSplit.shift();
      this.execEngineVersionSnapshot2 = snapShotSplit.join('.');
    }
    this.createdDate = blockchain.created;
    if (blockchain.created) {
      this.createdDateISO = new Date(blockchain.created).toISOString();
    }
    this.createdDateLocalTimezone = Intl.DateTimeFormat().resolvedOptions().timeZone;
    this.createdBy = blockchain.created_by;

    this.systemInfo.engine_type = this.execEngineName;
    this.systemInfo.engine_version = this.execEngineVersion;
    this.systemInfo.product_version = this.productVersion;
    this.systemInfo.blockchain_id = this.blockchainService.selectedBlockchain.id;
    this.systemInfo.created_date = new Date(blockchain.created).toISOString();
    this.systemInfo.created_by = blockchain.created_by;
  }

}
