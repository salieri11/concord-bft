/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, OnInit, ViewChild } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { ClrDatagrid, ClrDatagridComparatorInterface, ClrDatagridSortOrder } from '@clr/angular';

import { NodeInfo, NodeType, ClientNode, BlockchainNode, NodeCredentials } from '../shared/nodes.model';
import { DeployClientComponent } from '../deploy-client/deploy-client.component';
import { NodesService } from '../shared/nodes.service';
import { BlockchainService } from '../../blockchain/shared/blockchain.service';
import { ContractEngines } from '../../blockchain/shared/blockchain.model';
import { Personas } from '../../shared/persona.service';
import { FeatureFlagService } from '../../shared/feature-flag.service';
import { ZoneType } from '../../zones/shared/zones.model';
import { OrgService } from '../../orgs/shared/org.service';

class Sorter implements ClrDatagridComparatorInterface<ClientNode> {
  compare(a: ClientNode, b: ClientNode) {
    return a.group_name.localeCompare(b.group_name);
  }
}
@Component({
  selector: 'concord-node-list',
  templateUrl: './node-list.component.html',
  styleUrls: ['./node-list.component.scss']
})
export class NodeListComponent implements OnInit {
  @ViewChild('grid', {static: false}) grid: ClrDatagrid;
  @ViewChild('deployClient', {static: false}) deployClient: DeployClientComponent;

  committers: NodeInfo[];
  clients: ClientNode[];
  selected: any[] = [];
  batchStartEnabled: boolean;
  batchStopEnabled: boolean;
  type: NodeType;
  options = NodeType;
  personas = Personas;
  blockchainType: ContractEngines;
  engines = ContractEngines;
  cloudZone = ZoneType.VMC_AWS;
  sorter: Sorter = new Sorter();
  ascSort = ClrDatagridSortOrder.ASC;
  // ! temporary feature flag
  nodeDashboardEnabled: boolean = false;


  securePasswordEnabled: boolean = false;
  securePasswordModalShown: boolean = false;
  securePasswordFetching: boolean = false;
  securePasswordHidden: boolean = true;
  securePasswordNode: BlockchainNode;
  securePasswordData: NodeCredentials;
  securePasswordCached: { [nodeId: string]: NodeCredentials } = {};

  constructor(
    public nodesService: NodesService,
    private route: ActivatedRoute,
    private blockchainService: BlockchainService,
    private ff: FeatureFlagService,
    private orgService: OrgService
  ) {
    this.blockchainType = this.blockchainService.type;
    this.nodeDashboardEnabled = this.ff.check('node_dashboard');
    this.committers = this.nodesService.committers;
    this.clients = this.nodesService.clients;
  }

  ngOnInit() {
    this.route.params.subscribe(params => {
      this.type = params.nodeTypeOrId;
    });

    this.orgService.getList().subscribe(resp => {
      this.securePasswordEnabled = resp[0].organization_properties['secure-password'];
    });

  }

  openDeployClient() {
    this.deployClient.openModal();
  }

  downloadCert(node) {
    let cert;

    if (node.cert) {
      cert = node.cert;
    } else if (node.certificate) {
      cert = node.certificate;
    }

    const element = document.createElement('a');
    element.setAttribute('href', 'data:text/plain;charset=utf-8,' + encodeURIComponent(cert));
    element.setAttribute('download', node.name);

    element.style.display = 'none';
    document.body.appendChild(element);

    element.click();

    document.body.removeChild(element);
  }

  getNodeInfo(node) {
    return JSON.stringify(node);
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
    this.nodesService.getNodeCredentials(node).subscribe(credentials => {
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
