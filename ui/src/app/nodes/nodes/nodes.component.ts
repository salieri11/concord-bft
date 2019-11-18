/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component } from '@angular/core';

import { TranslateService } from '@ngx-translate/core';

import { NodeInfo } from '../shared/nodes.model';

@Component({
  selector: 'concord-nodes',
  templateUrl: './nodes.component.html',
  styleUrls: ['./nodes.component.scss']
})
export class NodesComponent {

  mockStats = {
    totalActiveNodes: 28458,
    inactiveNodes: 583,
    overallNodeHealth: .8742123,
    transactionsPerSecond: 4289,
    averageValidationTime: 1.98
  };

  members: NodeInfo[] = [];

  graphData: any[];

  dailyData = [
    {
      'name': 'Blocks',
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
      'name': 'Blocks',
      'series': [
        {
          'name': this.translateService.instant(`graph.days.${new Date('2018-12-07 10:30 AM').getDay()}`),
          'value': 12,
        },
        {
          'name': this.translateService.instant(`graph.days.${new Date('2018-12-08 11:30 AM').getDay()}`),
          'value': 15,
        },
        {
          'name': this.translateService.instant(`graph.days.${new Date('2018-12-09 01:30 PM').getDay()}`),
          'value': 17,
        },
        {
          'name': this.translateService.instant(`graph.days.${new Date('2018-12-10 02:30 PM').getDay()}`),
          'value': 14,
        },
        {
          'name': this.translateService.instant(`graph.days.${new Date('2018-12-11 07:30 PM').getDay()}`),
          'value': 18,
        },
        {
          'name': this.translateService.instant(`graph.days.${new Date('2018-12-12 07:30 PM').getDay()}`),
          'value': 18,
        }
      ]
    }
  ];

  monthlyData = [
    {
      'name': 'Blocks',
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

  constructor(private translateService: TranslateService) {
    this.graphData = this.dailyData;
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
}
