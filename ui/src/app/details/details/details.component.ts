/*
 * Copyright 2018-2020 VMware, all rights reserved.
 */
import { Component, OnInit } from '@angular/core';
import { BlockchainService } from '../../blockchain/shared/blockchain.service';
import { OrgService } from '../../orgs/shared/org.service';
import { NodesService } from '../../nodes/shared/nodes.service';
import { ContractEngines } from '../../blockchain/shared/blockchain.model';
import { ActivatedRoute } from '@angular/router';
import { ZonesService } from '../../zones/shared/zones.service';
import { Zone } from '../../zones/shared/zones.model';

// Only used for keeping tab button clients/commiter count less jumpy while loading
let lastKnownCommitters = [];
let lastKnownClients = [];

@Component({
  selector: 'concord-details',
  templateUrl: './details.component.html',
  styleUrls: ['./details.component.scss']
})
export class DetailsComponent implements OnInit {

  selectedTab = '';
  committers = lastKnownCommitters;
  clients = lastKnownClients;
  zones: {[id: string]: Zone} = {};
  zoneFirst: Zone = null;
  zonesSet: boolean = false;

  // Blockchain Info
  orgId = '';
  // TODO: get this info from actual API
  productVersion = '';
  createdBy = '';
  createdDate = 0;
  execEngineName = 'DAML Ledger';
  execEngineVersion = '_';
  concordName = 'Concord';
  concordVersion = 'Scalable BFT';

  constructor(
    public blockchainService: BlockchainService,
    private orgService: OrgService,
    private nodeService: NodesService,
    private zoneService: ZonesService,
    private route: ActivatedRoute,
  ) {
    const blockchain = this.blockchainService.selectedBlockchain;
    if (blockchain.blockchain_type === ContractEngines.DAML) {
      this.execEngineName = 'DAML Ledger';
    } else if (blockchain.blockchain_type === ContractEngines.ETH) {
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
      }
    }
    this.createdDate = blockchain.created;
    this.createdBy = blockchain.created_by;
    this.orgService.getList().subscribe(resp => {
      this.orgId = resp[0].organization_id;
    });
    this.nodeService.onNodeList.subscribe(_ => {
      this.committers = this.nodeService.committers;
      this.clients = this.nodeService.clients;
      lastKnownCommitters = this.committers;
      lastKnownClients = this.clients;
    });
    this.route.url.subscribe(url => {
      this.selectedTab = url[0].path;
    });
    this.zoneService.getZones().subscribe(zones => {
      for (const zone of zones) { this.zones[zone.id] = zone; }
      this.zonesSet = true;
    });
    this.nodeService.refreshAllNodesList();
  }

  ngOnInit() {
    // Nudges UI view to update about selected bc (even if it looks it's doing nothing)
    return this.blockchainService.selectedBlockchain;
  }

  getZoneInfo(zoneId: string) {
    if (!this.zonesSet || !this.zones[zoneId]) { return null; }
    const zone = this.zones[zoneId];
    const zoneType = zone.type === 'VMC_AWS' ? 'Cloud' : 'On-premises';
    return zone.name + ' (' + zoneType + ')';
  }

}
