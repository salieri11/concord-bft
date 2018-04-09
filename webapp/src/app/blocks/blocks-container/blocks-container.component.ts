/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';
import { HttpClient } from '@angular/common/http';

import { BlockListing, BlockListingBlock } from '../../shared/remote-interfaces';

/**
 * Displays a paginated listing of blocks
 */
@Component({
  selector: 'app-blocks-container',
  templateUrl: './blocks-container.component.html',
  styleUrls: ['./blocks-container.component.scss']
})
export class BlocksContainerComponent implements OnInit {
  blocks: BlockListingBlock[] = [];
  nextBlockUrl = '/api/athena/blocks';

  constructor(private httpClient: HttpClient) {}

  ngOnInit() {
    this.loadNextBlocks();
  }

  loadNextBlocks() {
    this.httpClient.get(this.nextBlockUrl).subscribe((data: BlockListing) => {
      this.blocks = data.blocks;
      // TODO: Abstract to class with hasNext(), next(), etc. Support pagination in URL directly. Need to support prev?
      if (data.next && data.next !== this.nextBlockUrl) {
        this.nextBlockUrl = data.next;
      } else {
        this.nextBlockUrl = undefined;
      }
    });
  }
}
