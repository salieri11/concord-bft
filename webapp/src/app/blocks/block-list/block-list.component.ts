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
  selector: 'athena-block-list',
  templateUrl: './block-list.component.html',
  styleUrls: ['./block-list.component.scss']
})
export class BlockListComponent implements OnInit {
  blocks: BlockListingBlock[] = [];
  nextBlockUrl: string;
  graphData: any[];

  days = ['Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'];

  dailyData = [
    {
      'name': 'Transactions',
      'series': [
        {
          'name': new Date('2018-12-07 10:30 AM'),
          'value': 3,
        },
        {
          'name': new Date('2018-12-07 11:30 AM'),
          'value': 4,
        },
        {
          'name': new Date('2018-12-07 01:30 PM'),
          'value': 5,
        },
        {
          'name': new Date('2018-12-07 02:30 PM'),
          'value': 9,
        },
        {
          'name': new Date('2018-12-07 07:30 PM'),
          'value': 2,
        }
      ]
    }
  ];

  weeklyData = [
    {
      'name': 'Transactions',
      'series': [
        {
          'name': this.days[new Date('2018-12-07 10:30 AM').getDay()],
          'value': 12,
        },
        {
          'name': this.days[new Date('2018-12-08 11:30 AM').getDay()],
          'value': 15,
        },
        {
          'name': this.days[new Date('2018-12-09 01:30 PM').getDay()],
          'value': 17,
        },
        {
          'name': this.days[new Date('2018-12-10 02:30 PM').getDay()],
          'value': 14,
        },
        {
          'name': this.days[new Date('2018-12-11 07:30 PM').getDay()],
          'value': 18,
        },
        {
          'name': this.days[new Date('2018-12-12 07:30 PM').getDay()],
          'value': 18,
        }
      ]
    }
  ];

  monthlyData = [
    {
      'name': 'Transactions',
      'series': [
        {
          'name': new Date('2018-12-07'),
          'value': 25,
        },
        {
          'name': new Date('2018-12-08'),
          'value': 36,
        },
        {
          'name': new Date('2018-12-09'),
          'value': 29,
        },
        {
          'name': new Date('2018-12-10'),
          'value': 50,
        },
        {
          'name': new Date('2018-12-11'),
          'value': 28,
        }
      ]
    }
  ];

  quarterlyData = [
    {
      'name': 'Transactions',
      'series': [
        {
          'name': new Date('2018-01-07'),
          'value': 46,
        },
        {
          'name': new Date('2018-01-17'),
          'value': 76,
        },
        {
          'name': new Date('2018-02-12'),
          'value': 36,
        },
        {
          'name': new Date('2018-03-07'),
          'value': 66,
        },
        {
          'name': new Date('2018-04-27'),
          'value': 48,
        }
      ]
    }
  ];

  constructor(private blocksService: BlocksService) {
    this.graphData = this.dailyData;
  }

  ngOnInit() {
    this.loadInitialBlocks();
  }

  getTodaysData() {
    this.graphData = this.dailyData;
  }

  getLastSevenDaysData() {
    this.graphData = this.weeklyData;
  }

  getLastThirtyDaysData() {
    this.graphData = this.monthlyData;
  }

  getLastNinetyDaysData() {
    this.graphData = this.quarterlyData;
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
