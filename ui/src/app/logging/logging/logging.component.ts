/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';
import { TranslateService } from '@ngx-translate/core';

import { LogApiService } from '../shared/log-api.service';
import { LogTaskCompletedResponse, LogListEntry, LogCountEntry } from '../shared/logging.model';

@Component({
  selector: 'concord-logging',
  templateUrl: './logging.component.html',
  styleUrls: ['./logging.component.scss']
})
export class LoggingComponent implements OnInit {
  logs: LogListEntry[] = [];
  logCounts: LogCountEntry[] = [];
  listLoading: boolean = true;
  graphData: { name: string, series: { name: Date, value: number }[] }[];
  heatMapData: { name: string, series: { name: string, value: number }[] }[];
  nextPageLink: string = null;

  constructor(private logApiService: LogApiService, private translate: TranslateService) { }

  ngOnInit() {
    const oneWeekAgo = new Date();
    oneWeekAgo.setDate(oneWeekAgo.getDate() - 7);
    const start = oneWeekAgo.getTime();
    const end = new Date().getTime();

    this.fetchLogs(start, end);
    this.fetchLogCounts(start, end);
  }

  fetchLogs(start: number, end: number) {
    this.listLoading = true;
    this.logApiService.postToTasks(start, end).subscribe((resp) => {
      this.pollLogStatus(resp.documentSelfLink, 'logs', this.onFetchLogsComplete.bind(this));
    });
  }

  fetchLogCounts(start: number, end: number) {
    this.logApiService.postToTasksCount(start, end).subscribe((resp) => {
      this.pollLogStatus(resp.documentSelfLink, 'logCounts', (logResp) => {
        this.logCounts = logResp.logQueryResults;
        this.parseCounts();
      });
    });
  }

  onFetchNextPage() {
    this.listLoading = true;
    this.logApiService.fetchLogStatus(this.nextPageLink).subscribe((resp) => {
      this.pollLogStatus(resp.documentSelfLink, 'logs', this.onFetchLogsComplete.bind(this));
    });
  }

  private onFetchLogsComplete(logResp: LogTaskCompletedResponse) {
    this.logs = logResp.logQueryResults;
    this.nextPageLink = logResp.nextPageLink;
    this.listLoading = false;
  }

  private pollLogStatus(link: string, type: string, callback: (n: LogTaskCompletedResponse) => void) {
    this.logApiService.fetchLogStatus(link).subscribe((logResponse) => {
      if (logResponse.taskInfo.stage === 'FINISHED') {
        callback(logResponse);
      } else {
        setTimeout(() => {
          this.pollLogStatus(link, type, callback);
        }, 1000);
      }
    });
  }

  private parseCounts() {
    const parsedCounts = [];
    const heatMapData = [];
    // An interstitial object to hold the day/hour counts before converting it to the required formatting for a heat map
    // https://swimlane.gitbook.io/ngx-charts/examples/heat-map-chart#data-format
    const heatMapObj = {
      0: {},
      1: {},
      2: {},
      3: {},
      4: {},
      5: {},
      6: {},
    };
    this.logCounts.map((count) => {
      const date = new Date(count.timestamp);
      // Parse the day and hour from the timestamp
      const day = date.getDay();
      const hour = date.getHours();
      const value = parseInt(count['count(*)'], 10);
      // Add the name and value object to parsedCounts as is required for area chart data formatting
      parsedCounts.push({ 'name': date, 'value': value });
      // Add the value for the given hour to the existing hour key or create it if necessary
      // This should scale as long as the interval for log counts is an hour or less
      heatMapObj[day][hour] = heatMapObj[day][hour] + value || value;
    });
    this.graphData = [{
      'name': 'Logs',
      'series': parsedCounts
    }];
    // Loop over each 'day' 0 (Sunday) - 6 (Saturday)
    Object.keys(heatMapObj).forEach((day) => {
      const hoursArray = [];
      // Loop over each 'hour' 0 - 23
      Object.keys(heatMapObj[day]).forEach((hour) => {
        // Add the name and value object as required by heat map chart data formatting
        hoursArray.push({ name: hour.toString(), value: heatMapObj[day][hour] });
      });
      // add the hours series data to the day. Translate the day abbreviation from the number 0 - 6
      heatMapData.push({ name: this.translate.instant(`logging.heatMap.days.${day.toString()}`), series: hoursArray.reverse() });
    });
    // Move sunday to the end of the array
    const sundayData = heatMapData.shift();
    heatMapData.push(sundayData);
    this.heatMapData = heatMapData;
  }
}
