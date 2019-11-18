/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, Input, OnInit, OnDestroy } from '@angular/core';

import { Block } from '../shared/blocks.model';
import { BlocksService } from '../shared/blocks.service';
import { mainRoutes } from '../../shared/urls.model';
// import { RouteService } from '../../shared/route.service';
import { BlockchainService } from '../../blockchain/shared/blockchain.service';
import { ActivatedRoute } from '@angular/router';
import { Subscription } from 'rxjs';

@Component({
  selector: 'concord-block-details',
  templateUrl: './block-details.component.html',
  styleUrls: ['./block-details.component.scss']
})
export class BlockDetailsComponent implements OnInit, OnDestroy {
  @Input() blockNumber: number;

  block: Block;
  paramsSub: Subscription;

  constructor(
    private blocksService: BlocksService,
    private blockchainService: BlockchainService,
    private route: ActivatedRoute,
    ) {}

  ngOnInit() {
    this.loadBlock(this.blockNumber);
    this.paramsSub = this.route.params.subscribe((param) => {
      if (param['blockNumber'] === undefined) { return; }
      this.blockNumber = parseInt(param['blockNumber'], 10);
      this.loadBlock(this.blockNumber);
    });
  }

  ngOnDestroy() {
    if (this.paramsSub) { this.paramsSub.unsubscribe(); }
  }

  loadBlock(blockNumber: number) {
    this.blocksService.getBlock(blockNumber).subscribe(response => {
      this.block = response;
    });
  }

  getBlockPageRouterLink(blockNumber) {
    if (!blockNumber && blockNumber !== 0) { return []; }
    return ['/' + this.blockchainService.blockchainId, mainRoutes.blocks, blockNumber];
  }

  timestampToISOString(t) {
    return new Date(t).toISOString();
  }
}
