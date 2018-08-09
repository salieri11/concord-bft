/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';

import { BlockListingBlock } from '../../blocks/shared/blocks.model';
import { TransactionsService } from '../../transactions/shared/transactions.service';

@Component({
  selector: 'athena-node',
  templateUrl: './node.component.html',
  styleUrls: ['./node.component.scss']
})
export class NodeComponent implements OnInit {

  nodeName = '';
  blocks: BlockListingBlock[];
  recentTransactions: any[] = [];

  mockGraphData = [
    {
      'name': 'Blocks',
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

  mockMapData = {
    'type': 'FeatureCollection',
    'features': [
      {
        'type': 'Feature',
        'geometry': {
          'type': 'Point',
          'coordinates': [
            -6.266155,
            53.350140
          ]
        },
        'properties': {
          'codeName': 'eu-west-1',
          'location': 'Ireland, EU',
          'nodes': [
            {
              'id': 'athena1',
              'consortiumName': 'Consortium 1',
              'blockchainType': 'Athena',
              'status': 'Healthy'
            }
          ]
        }
      }
    ]
  };

  constructor(private transactionsService: TransactionsService, private route: ActivatedRoute) { }

  ngOnInit() {
    this.route.params.subscribe(params => {
      this.nodeName = params.id;
      this.mockMapData.features[0].properties.nodes[0].id = params.id;
    });

    this.transactionsService.getRecentTransactions().subscribe((resp) => {
      this.recentTransactions = resp;
    });
  }
}
