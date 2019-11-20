/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, Input, OnInit } from '@angular/core';
import { TranslateService } from '@ngx-translate/core';

@Component({
  selector: 'concord-log-graph',
  templateUrl: './log-graph.component.html',
  styleUrls: ['./log-graph.component.scss']
})
export class LogGraphComponent implements OnInit {
  @Input('graphData') graphData: any[];
  @Input('interval') interval: number;
  @Input('xAxisTickFormatting') xAxisTickFormatting: (date) => string;

  // graph options
  showXAxis = true;
  showYAxis = true;
  gradient = true;
  schemeType = 'ordinal';
  showGridLines = true;
  colorScheme = {
    domain: ['#0094d2']
  };

  constructor(private translate: TranslateService) { }

  ngOnInit() {
  }

  getStartTimeText(model) {
    if (model) {
      const pastDate = new Date(model.name.valueOf() - this.interval);
      return this.translate.instant('logging.bar.fromDate', { fromDate: pastDate.toUTCString() });
    } else {
      return this.translate.instant('logging.bar.fromDate', { fromDate: '' });
    }

  }

  getEndTimeText(model) {
    if (model) {
      return this.translate.instant('logging.bar.toDate', { toDate: model.name.toUTCString() });
    } else {
      return this.translate.instant('logging.bar.toDate', { toDate: '' });
    }
  }

}
