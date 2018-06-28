/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';

import { AthenaApiService } from '../../shared/athena-api.service';
import { Block } from '../../shared/remote-interfaces';

/**
 * Displays a single block's details
 */
@Component({
  selector: 'app-block',
  templateUrl: './block.component.html',
  styleUrls: ['./block.component.scss']
})
export class BlockComponent implements OnInit {
  block: Block;

  constructor(private athenaApiService: AthenaApiService, private route: ActivatedRoute) {}

  ngOnInit() {
    this.route.params.subscribe(params => {
      this.loadBlock(params.blockNumber);
    });
  }

  loadBlock(blockNumber) {
    this.athenaApiService.getBlock(blockNumber).subscribe(block => {
      this.block = block;
    });
  }
}
