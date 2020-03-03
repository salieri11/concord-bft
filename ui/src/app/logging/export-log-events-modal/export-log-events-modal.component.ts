/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, Input, OnInit } from '@angular/core';
import { json2csv } from 'json-2-csv';

import { LogApiService } from '../shared/log-api.service';
import { LogListEntry, LogTaskCompletedResponse } from '../shared/logging.model';
import { generateDownload } from '../../shared/download-helpers';
import { ErrorAlertService } from '../../shared/global-error-handler.service';

// The max number of logs records that are exportable.
// More than this may be unreliable.
const LOG_THRESHOLD = 15000;

@Component({
  selector: 'concord-export-log-events-modal',
  templateUrl: './export-log-events-modal.component.html',
  styleUrls: ['./export-log-events-modal.component.scss']
})
export class ExportLogEventsModalComponent implements OnInit {
  @Input('startTime') startTime: number;
  @Input('endTime') endTime: number;
  @Input('totalCount') totalCount: number;
  @Input('replicaId') replicaId: number;
  @Input('serviceName') serviceName: string;
  @Input('search') search: string;
  @Input('logLevels') logLevels: string[];

  isLoading: boolean = false;
  exportLogs: LogListEntry[] = [];
  isOpen: boolean = false;
  constructor(private logApiService: LogApiService, private errorService: ErrorAlertService) { }

  ngOnInit() {
  }

  get logCountAboveThreshold() {
    return this.totalCount > LOG_THRESHOLD;
  }

  get percentageComplete() {
    const percentage = (this.exportLogs.length / this.totalCount);

    return percentage > 1 ? 1 : percentage;
  }

  exportToCsv() {
    this.isLoading = true;
    this.exportLogs = [];

    this.logApiService.postToTasks(
      this.startTime,
      this.endTime,
      this.replicaId.toString(),
      this.logLevels,
      this.search,
      this.serviceName,
      1000
    ).subscribe((taskResponse) => {
      this.pollLogStatus(taskResponse.documentSelfLink, 'logs', this.buildExportLogs.bind(this));
    });
  }

  onClose() {
    this.isOpen = false;
    this.isLoading = false;
    this.exportLogs = [];
  }

  public open() {
    this.isOpen = true;
  }

  private buildExportLogs(logResponse) {
    this.exportLogs = this.exportLogs.concat(logResponse.logQueryResults);

    if (logResponse.nextPageLink) {
      this.logApiService.fetchLogStatus(logResponse.nextPageLink).subscribe((logStatusResp) => {
        this.pollLogStatus(logStatusResp.documentSelfLink, 'logs', this.buildExportLogs.bind(this));
      });
    } else {
      this.isLoading = false;
      json2csv(this.exportLogs, (err, csv) => {
        if (err) {
          this.errorService.add(err);
        } else {
          // download log
          generateDownload(`${this.startTime}-${this.endTime}-log-events.csv`, csv);
        }
      });
      this.onClose();
    }
  }

  private pollLogStatus(link: string, type: string, callback: (n: LogTaskCompletedResponse) => void) {
    this.logApiService.fetchLogStatus(link).subscribe((logResponse) => {
      if (logResponse.taskInfo.stage === 'FINISHED') {
        callback(logResponse);
      } else if (this.isOpen) {
        setTimeout(() => {
          this.pollLogStatus(link, type, callback);
        }, 1000);
      }
    });
  }
}
