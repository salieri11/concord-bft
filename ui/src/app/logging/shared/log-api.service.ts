/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Inject, Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';
import { mergeMap } from 'rxjs/operators';

import { LOG_API_PREFIX } from '../../shared/shared.config';
import { CspApiService } from '../../shared/csp-api.service';
import { LogTaskResponse, LogTaskCompletedResponse, LogTaskParams, LogTimePeriod, LogCountEntry } from './logging.model';
import { ONE_MINUTE, FIFTEEN_MINUTES } from './logging.constants';

@Injectable({
  providedIn: 'root'
})
export class LogApiService {

  constructor(@Inject(LOG_API_PREFIX) private logApiPrefix: string, private httpClient: HttpClient, private cspApi: CspApiService) {}

  postToTasks(start: number, end: number, rows: number = 20): Observable<LogTaskResponse> {
    const logQuery = {
      logQuery: 'SELECT * FROM logs ORDER BY ingest_timestamp DESC',
      start: start,
      end: end,
      rows: rows
    };
    return this.logQueryTask(logQuery);
  }

  postToTasksCount(start: number, end: number, interval: number): Observable<LogTaskResponse> {
    const logQuery = {
      logQuery: `SELECT COUNT(*), timestamp FROM logs GROUP BY bucket(timestamp, ${interval}, ${start}, ${end}) ORDER BY timestamp DESC`,
      start: start,
      end: end
    };
    return this.logQueryTask(logQuery);
  }

  postToPureCount(start: number, end: number): Observable<LogTaskResponse> {
    const logQuery = {
      logQuery: 'SELECT COUNT(*) FROM logs',
      start: start,
      end: end
    };
    return this.logQueryTask(logQuery);
  }

  fetchLogStatus(path: string): Observable<LogTaskCompletedResponse> {
    return this.cspApi.fetchToken().pipe(mergeMap(((tokenResp: any) => {
      return this.httpClient.get<LogTaskCompletedResponse>(`${this.logApiPrefix}${path}`, {
        headers: new HttpHeaders()
          .set('Authorization', `Bearer ${tokenResp.access_token}`)
      });
    })));
  }

  padHeatMapData(heatMapObj: {[hour: number]: number}): void {
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

  private logQueryTask(logQuery: LogTaskParams): Observable<LogTaskResponse> {
    return this.cspApi.fetchToken().pipe(mergeMap(((tokenResp: any) => {
      return this.httpClient.post<LogTaskResponse>(`${this.logApiPrefix}/ops/query/log-query-tasks`, logQuery, {
        headers: new HttpHeaders()
          .set('Authorization', `Bearer ${tokenResp.access_token}`)
      });
    })));
  }
}

