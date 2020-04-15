/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, OnInit, ViewChild } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { ClrDatagrid } from '@clr/angular';

import { NodeInfo, NodeType, ClientNode } from '../shared/nodes.model';
import { DeployClientComponent } from '../deploy-client/deploy-client.component';
import { NodesService } from '../shared/nodes.service';
import { BlockchainService } from '../../blockchain/shared/blockchain.service';
import { ContractEngines } from '../../blockchain/shared/blockchain.model';
import { Personas } from '../../shared/persona.service';

@Component({
  selector: 'concord-node-list',
  templateUrl: './node-list.component.html',
  styleUrls: ['./node-list.component.scss']
})
export class NodeListComponent implements OnInit {
  @ViewChild('grid', {static: false}) grid: ClrDatagrid;
  @ViewChild('deployClient', {static: false}) deployClient: DeployClientComponent;

  nodes: NodeInfo[];
  clients: ClientNode[];
  selected: any[] = [];
  batchStartEnabled: boolean;
  batchStopEnabled: boolean;
  type: NodeType;
  options = NodeType;
  personas = Personas;
  blockchainType: ContractEngines;
  engines = ContractEngines;

  constructor(
    private nodesService: NodesService,
    private route: ActivatedRoute,
    private blockchainService: BlockchainService
  ) {
    this.blockchainType = this.blockchainService.type;
  }

  ngOnInit() {
    this.route.params.subscribe(params => {
      this.type = params.nodeType;

      switch (this.type) {
        case NodeType.committers:
          this.loadCommitters();
          break;

        case NodeType.clients:
          this.loadClients();
          break;

        default:
          this.loadCommitters();
          break;
      }


    });

    this.nodesService.tasksUpdated.subscribe(() => {
      this.loadClients();
    });

    //
    // Commenting out node start stop functionality until it's ready to be implemented on the backend.
    //
    // this.confirm.confirmed.subscribe(response => {
    //   console.log(response);
    //   this[response.action](response.node);
    // });

    // this.grid.selectedChanged
    //   .subscribe(selections => this.handleSelections(selections));
  }

  loadCommitters() {
    this.nodesService.getList()
    .subscribe(nodes => this.nodes = nodes.nodes);
  }

  loadClients() {
    this.nodesService.getClients()
    .subscribe(clients => this.clients = clients);
  }

  openDeployClient() {
    this.deployClient.openModal();
  }

  //
  // Commenting out node start stop functionality until it's ready to be implemented on the backend.
  //
  // confirmAction(action: string, node: Node | Node[]) {
  //   this.confirm.open(action, node);
  // }

  // start(node: Node | Node[]) {
  //   this.nodesService.action('start', node).subscribe(
  //     response => this.handleResponse(response),
  //     error => this.handleError(error)
  //   );
  // }

  // stop(node: Node | Node[]) {
  //   this.nodesService.action('stop', node).subscribe(
  //     response => this.handleResponse(response),
  //     error => this.handleError(error)
  //   );
  // }

  // restart(node: Node | Node[]) {
  //   this.nodesService.action('restart', node).subscribe(
  //     response => this.handleResponse(response),
  //     error => this.handleError(error)
  //   );
  // }

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

  //
  // Commenting out node start stop functionality until it's ready to be implemented on the backend.
  //
  // private handleResponse(response) {
  //   console.log('response')
  //   console.log(response)
  // }

  // private handleError(error) {
  //   console.log('error')
  //   console.log(error)
  // }

  // private handleSelections(selections: any[]) {
  //   this.batchStartEnabled = false;
  //   this.batchStopEnabled = false;
  //   const liveSel = selections.filter(sel => sel.state === 'live');

  //   if (liveSel.length) {
  //     this.batchStopEnabled = true;
  //   } else if (selections.length) {
  //     this.batchStartEnabled = true;
  //   }
  // }

}