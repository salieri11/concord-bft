/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';

import { BlockListing, BlockListingBlock } from '../shared/blocks.model';
import { BlocksService } from '../shared/blocks.service';

/**
 * Displays a paginated listing of blocks
 */
@Component({
  selector: 'athena-blocks-list',
  templateUrl: './blocks-list.component.html',
  styleUrls: ['./blocks-list.component.scss']
})
export class BlocksListComponent implements OnInit {
  blocks: BlockListingBlock[] = [];
  nextBlockUrl: string;

  constructor(private blocksService: BlocksService) {}

  ngOnInit() {
    this.loadInitialBlocks();
  }

  loadInitialBlocks() {
    this.blocksService.getBlocks(1000).subscribe(response => this.handleBlocksResponse(response));
  }

  loadNextBlocks() {
    this.blocksService.getBlocksByUrl(this.nextBlockUrl).subscribe(response => this.handleBlocksResponse(response));
  }

  handleBlocksResponse(response: BlockListing) {
    this.blocks = response.blocks;
    // TODO: Abstract to class with hasNext(), next(), etc. Support pagination in URL directly. Need to support prev?
    this.nextBlockUrl = response.next !== this.nextBlockUrl ? this.nextBlockUrl = response.next : undefined;
  }
}
