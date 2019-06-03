/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, OnInit, ViewChild } from '@angular/core';
import { Node } from '../shared/nodes.model';
import { NodesService } from '../shared/nodes.service';

import { ConfirmActionComponent } from './../confirm-action/confirm-action.component';

@Component({
  selector: 'concord-node-list',
  templateUrl: './node-list.component.html',
  styleUrls: ['./node-list.component.scss']
})
export class NodeListComponent implements OnInit {
  @ViewChild('confirm') confirm: ConfirmActionComponent;

  nodes: Node[];
  selected: any[] = [];

  constructor(private nodesService: NodesService) { }

  ngOnInit() {
    this.loadNodes();

    this.confirm.confirmed.subscribe(response => {
      console.log(response);
      this[response.action](response.node);
    });
  }

  loadNodes() {
    this.nodesService.getNodes()
    .subscribe(nodes => this.nodes = nodes.nodes);
  }

  confirmAction(action: string, node: Node | Node[]) {
    this.confirm.open(action, node);
  }

  start(node: Node | Node[]) {
    this.nodesService.action('start', node).subscribe(
      response => this.handleResponse(response),
      error => this.handleError(error)
    );
  }

  stop(node: Node | Node[]) {
    this.nodesService.action('stop', node).subscribe(
      response => this.handleResponse(response),
      error => this.handleError(error)
    );
  }

  restart(node: Node | Node[]) {
    this.nodesService.action('restart', node).subscribe(
      response => this.handleResponse(response),
      error => this.handleError(error)
    );
  }

  downloadCert() {

  }

  private handleResponse(response) {
    console.log('response')
    console.log(response)
  }

  private handleError(error) {
    console.log('error')
    console.log(error)
  }

}
