/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';

@Component({
  selector: 'concord-nodes',
  templateUrl: './nodes.component.html',
  styleUrls: ['./nodes.component.scss']
})
export class NodesComponent implements OnInit {

  viewTypeList: boolean = true; // show nodes-list or dashboard
  replicaId: string;

  constructor(
    private route: ActivatedRoute,
  ) {}

  ngOnInit() {
    this.route.params.subscribe(params => {
      // UUID?
      if (params['nodeTypeOrId'] && params['nodeTypeOrId'].toString().length === 36) {
        this.replicaId = params['nodeTypeOrId'];
        this.viewTypeList = false;
      } else { // Node types (Committer, Client, Object Store, etc.)
        this.viewTypeList = true;
      }
    });
  }
}
