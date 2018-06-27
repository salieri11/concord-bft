/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';

import { Block } from '../shared/blocks.model';
import { BlocksService } from '../shared/blocks.service';

/**
 * Displays a single block's details
 */
@Component({
  selector: 'athena-block-detail-container',
  templateUrl: './block-detail-container.component.html',
  styleUrls: ['./block-detail-container.component.scss']
})
export class BlockDetailContainerComponent implements OnInit {
  block: Block;

  constructor(private blocksService: BlocksService, private route: ActivatedRoute) {}

  ngOnInit() {
    this.route.params.subscribe(params => {
      this.loadBlock(params.blockNumber);
    });
  }

  loadBlock(blockNumber) {
    this.blocksService.getBlock(blockNumber).subscribe(block => {
      this.block = block;
    });
  }
}
