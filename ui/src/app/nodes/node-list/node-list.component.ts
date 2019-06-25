/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';
import { NodesResponse } from '../shared/nodes.model';
import { NodesService } from '../shared/nodes.service';

@Component({
  selector: 'concord-node-list',
  templateUrl: './node-list.component.html',
  styleUrls: ['./node-list.component.scss']
})
export class NodeListComponent implements OnInit {
  nodes: NodesResponse;

  constructor(private nodesService: NodesService) { }

  ngOnInit() {
    this.loadNodes();
  }

  loadNodes() {
    this.nodesService.getNodes()
    .subscribe(nodes => this.nodes = nodes);
  }

}
