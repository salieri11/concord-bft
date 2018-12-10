/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, Input, OnInit } from '@angular/core';

import { Block } from '../shared/blocks.model';
import { BlocksService } from '../shared/blocks.service';

@Component({
  selector: 'concord-block-details',
  templateUrl: './block-details.component.html',
  styleUrls: ['./block-details.component.scss']
})
export class BlockDetailsComponent implements OnInit {
  @Input() blockNumber: number;

  block: Block;

  constructor(private blocksService: BlocksService) {
  }

  ngOnInit() {
    this.loadBlock(this.blockNumber);
  }

  loadBlock(blockNumber: number) {
    this.blocksService.getBlock(blockNumber).subscribe(response => {
      this.block = response;
    });
  }
}
