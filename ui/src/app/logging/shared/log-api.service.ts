/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';

import { BlockchainService } from './../../blockchain/shared/blockchain.service';
import { LogTaskResponse, LogTaskCompletedResponse, LogTaskParams, LogTimePeriod, LogCountEntry } from './logging.model';
import { ONE_MINUTE, FIFTEEN_MINUTES } from './logging.constants';

@Injectable({
  providedIn: 'root'
})
export class LogApiService {

  constructor(
    private httpClient: HttpClient,
    private blockchainService: BlockchainService
  ) { }

  postToTasks(
    start: number,
    end: number,
    nodes: string[],
    level: string[],
    search: string,
    service_names: string[],
    rows: number = 100): Observable<LogTaskResponse> {
    const query = `SELECT * FROM logs ${this.getWhereClause(level, service_names, search, nodes)}ORDER BY ingest_timestamp DESC`;

    const logQuery = {
      logQuery: query,
      start: start,
      end: end,
      rows: rows
    };
    return this.logQueryTask(logQuery);
  }

  postToTasksCount(
    start: number,
    end: number,
    nodes: string[],
    level: string[],
    search: string,
    service_names: string[],
    interval: number): Observable<LogTaskResponse> {
    const query = `SELECT COUNT(*), timestamp FROM logs ` +
      `${this.getWhereClause(level, service_names, search, nodes)}` +
      `GROUP BY bucket(timestamp, ${interval}, ${start}, ${end}) ORDER BY timestamp DESC`;

    const logQuery = {
      logQuery: query,
      start: start,
      end: end
    };
    return this.logQueryTask(logQuery);
  }

  postToPureCount(
    start: number,
    end: number,
    nodes: string[],
    level: string[],
    search: string,
    service_names: string[]): Observable<LogTaskResponse> {
    const query = `SELECT COUNT(*) FROM logs ${this.getWhereClause(level, service_names, search, nodes)}`;

    const logQuery = {
      logQuery: query,
      start: start,
      end: end
    };
    return this.logQueryTask(logQuery);
  }

  getWhereClause(
    level: string[],
    service_names: string[],
    search: string,
    nodes: string[]
  ) {
    let where = '';
    const flatNodes = nodes.flatMap(obj => obj.id);
    // Start our where clause if needed
    if (level.length || service_names.length || search.length || flatNodes.length) {
      where = 'WHERE';
    } else {
      return where;
    }

    where = this.logQueryListHelper(where, 'service_name', service_names);
    where = this.logQueryListHelper(where, 'level', level);
    where = this.logQueryListHelper(where, 'replica_id', flatNodes);

    if (search.length) {
      // If other clause has been added, then we need an AND
      if (where.length > 5) {
        where += 'AND';
      }

      where += ` text = '${search}' `;
    }

    return where;
  }

  fetchLogStatus(path: string): Observable<LogTaskCompletedResponse> {
    return this.httpClient.get<LogTaskCompletedResponse>(`api/lint${path}`);
  }

  padHeatMapData(heatMapObj: { [hour: number]: number }): void {
    Object.keys(heatMapObj).map((hour) => {
      for (let m = 0; m <= 50; m += 10) {
        if (heatMapObj[hour][m] === undefined) {
          heatMapObj[hour][m] = 0;
        }
      }
    });
  }

  padLogCounts(counts: LogCountEntry[], startTime: number, endTime: number, timePeriod: LogTimePeriod): LogCountEntry[] {
    // Log intelligence may not return values for all of the time intervals within a request. Here, we pad the
    // counts to ensure we always have a value for every interval in the selected range
    let roundedEndDate;
    let loopDateTime;
    const paddedCountsObject = {};
    // If all time periods are returned from log intelligence, time periods less than or equal to fifteen minutes
    // will return the first timestamp as the specified end time minus the interval. Otherwise, it will return the
    // first timestamp as the closest minute rounded up minus the interval
    if (timePeriod.value <= FIFTEEN_MINUTES) {
      roundedEndDate = new Date(endTime - timePeriod.interval);
    } else {
      roundedEndDate = new Date((Math.ceil(endTime / ONE_MINUTE) * ONE_MINUTE) - timePeriod.interval);
    }

    loopDateTime = roundedEndDate.valueOf();
    while (loopDateTime >= startTime) {
      // Starting at the initial time stamp, add a count for each time in the range based on the specified interval
      let dateString = new Date(loopDateTime).toISOString();
      // Due to the way log intelligence calculates the timestamp, we need to remove the seconds decimals
      // if the selected time period value is greater than fifteen minutes
      dateString = timePeriod.value > FIFTEEN_MINUTES ? dateString.replace('.000', '') : dateString;
      paddedCountsObject[dateString] = 0;
      loopDateTime = loopDateTime - timePeriod.interval;
    }

    counts.map((count) => {
      paddedCountsObject[count.timestamp] = parseInt(count['count(*)'], 10);
    });

    return Object.keys(paddedCountsObject).map((key) => {
      return {
        timestamp: key,
        'count(*)': paddedCountsObject[key]
      };
    });
  }

  private logQueryListHelper(queryString: string, key: string, list: string[]): string {
    if (list.length) {
      let listQuery = '';

      // Create our string
      list.forEach((l, i) => {
        // Add an or statement if more than one level
        if (i !== 0) {
          listQuery += ' OR ';
        }

        listQuery += `${key} = '${l}'`;
      });

      // If other clause has been added, then we need an AND
      if (queryString.length > 5) {
        queryString += 'AND';
      }

      queryString += ` (${listQuery}) `;
    }

    return queryString;
  }

  private logQueryTask(logQuery: LogTaskParams): Observable<LogTaskResponse> {

    return this.httpClient.post<LogTaskResponse>(
      `api/lint/ops/query/log-query-tasks?blockchain_id=${this.blockchainService.blockchainId}`,
      logQuery
    );
  }
}

