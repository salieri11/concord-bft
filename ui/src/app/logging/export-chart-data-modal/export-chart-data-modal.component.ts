/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, Input, OnInit } from '@angular/core';
import { json2csv } from 'json-2-csv';

import { LogCountEntry } from '../shared/logging.model';
import { generateDownload } from '../../shared/download-helpers';
import { ErrorAlertService } from '../../shared/global-error-handler.service';

@Component({
  selector: 'concord-export-chart-data-modal',
  templateUrl: './export-chart-data-modal.component.html',
  styleUrls: ['./export-chart-data-modal.component.scss']
})
export class ExportChartDataModalComponent implements OnInit {
  @Input('chartData') chartData: LogCountEntry[];
  @Input('startTime') startTime: number;
  @Input('endTime') endTime: number;
  isOpen: boolean = false;
  constructor(private errorService: ErrorAlertService) { }

  ngOnInit() {
  }

  onClose() {
    this.isOpen = false;
  }

  exportToCsv() {
    // remove asterisk from counts and download the csv
    const chartData = this.chartData.map((item) => {
      return {
        count: item['count(*)'],
        timestamp: item['timestamp']
      };
    });

    json2csv(chartData, (err, csv) => {
      if (err) {
        this.errorService.add(err);
      } else {
        // download log
        generateDownload(`${this.startTime}-${this.endTime}-log-chart-data.csv`, csv);
        this.onClose();
      }
    });

  }

  public open() {
    this.isOpen = true;
  }
}
