/*
 * Copyright 2018-2020 VMware, all rights reserved.
 */
import { Component, OnInit } from '@angular/core';
import { BlockchainService } from '../../blockchain/shared/blockchain.service';
import { OrgService } from '../../orgs/shared/org.service';
import { NodesService } from '../../nodes/shared/nodes.service';
import { ContractEngines } from '../../blockchain/shared/blockchain.model';
import { ActivatedRoute } from '@angular/router';
import { Zone, ZoneType } from '../../zones/shared/zones.model';
import { BlockchainNode, NodeCredentials } from '../../nodes/shared/nodes.model';
import { Personas } from '../../shared/persona.service';

// Only used for keeping tab button clients/commiter count less jumpy while loading
let lastKnownCommitters = [];
let lastKnownClients = [];

@Component({
  selector: 'concord-details',
  templateUrl: './details.component.html',
  styleUrls: ['./details.component.scss']
})
export class DetailsComponent implements OnInit {

  personas = Personas;

  selectedTab = '';
  committers = lastKnownCommitters;
  clients = lastKnownClients;
  zones: {[id: string]: Zone} = {};
  zoneFirst: Zone = null;
  blockchainType: ContractEngines;

  securePasswordEnabled: boolean = false;
  securePasswordModalShown: boolean = false;
  securePasswordFetching: boolean = false;
  securePasswordHidden: boolean = true;
  securePasswordNode: BlockchainNode;
  securePasswordData: NodeCredentials;
  securePasswordCached: {[nodeId: string]: NodeCredentials } = {};

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
  concordVersion = 'Scalable BFT';

  ready: boolean = false;
  fetchedList = { organizations: false, nodes: false, zones: false };

  constructor(
    public blockchainService: BlockchainService,
    private orgService: OrgService,
    private nodeService: NodesService,
    private route: ActivatedRoute,
  ) {
    this.route.url.subscribe(url => {
      this.selectedTab = url[0].path;
    });
    this.orgService.getList().subscribe(resp => {
      this.orgId = resp[0].organization_id;
      this.securePasswordEnabled = resp[0].organization_properties['secure-password'];
      this.fetchedList.organizations = true;
      this.ready = true;
    });
    this.committers = this.nodeService.committers;
    this.clients = this.nodeService.clients;
    lastKnownCommitters = this.committers;
    lastKnownClients = this.clients;
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
  }

  fetchSecurePassword(node: BlockchainNode) {
    const t1 = Date.now();
    const minLoadingTime = 350; // ms
    this.securePasswordNode = node;
    this.securePasswordModalShown = true;
    this.securePasswordFetching = true;
    const finalizeNodeCredentialInfo = (credentials: NodeCredentials) => {
      this.securePasswordFetching = false;
      this.securePasswordHidden = true;
      this.securePasswordData = credentials;
    };
    const cachedSecurePassword = this.securePasswordCached[node.id];
    if (cachedSecurePassword) { return finalizeNodeCredentialInfo(cachedSecurePassword); }
    this.nodeService.getNodeCredentials(node).subscribe(credentials => {
      this.securePasswordCached[node.id] = credentials;
      const t2 = Date.now();
      // Give minimum half-second loading time to give more consistent UX feel
      // (for both slow/fast server response)
      if (t2 - t1 > minLoadingTime) {
        finalizeNodeCredentialInfo(credentials);
      } else {
        setTimeout(() => { finalizeNodeCredentialInfo(credentials); }, minLoadingTime - (t2 - t1));
      }
    });
  }

  securePasswordModalClose() {
    this.securePasswordModalShown = false;
    this.securePasswordFetching = false;
  }

  securePasswordHiddenToggle() {
    this.securePasswordHidden = !this.securePasswordHidden;
  }

}
