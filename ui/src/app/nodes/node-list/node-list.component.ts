/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, OnInit, ViewChild } from '@angular/core';
import { ClrDatagrid } from '@clr/angular';

import { NodeInfo } from '../shared/nodes.model';
import { NodesService } from '../shared/nodes.service';

@Component({
  selector: 'concord-node-list',
  templateUrl: './node-list.component.html',
  styleUrls: ['./node-list.component.scss']
})
export class NodeListComponent implements OnInit {
  @ViewChild('grid', {static: false}) grid: ClrDatagrid;

  nodes: NodeInfo[];
  selected: any[] = [];
  batchStartEnabled: boolean;
  batchStopEnabled: boolean;

  constructor(private nodesService: NodesService) { }

  ngOnInit() {
    this.loadNodes();

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

  loadNodes() {
    this.nodesService.getNodes()
    .subscribe(nodes => this.nodes = nodes.nodes);
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
    const element = document.createElement('a');
    element.setAttribute('href', 'data:text/plain;charset=utf-8,' + encodeURIComponent(node.certificate));
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
