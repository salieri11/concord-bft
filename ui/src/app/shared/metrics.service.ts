/*
 * Copyright 2018-2020 VMware, all rights reserved.
 */
import { Injectable } from '@angular/core';
import { Observable, of } from 'rxjs';
import { HttpClient } from '@angular/common/http';
import { catchError } from 'rxjs/operators';
import { Apis } from './urls.model';
import { testController } from '../../test.controller'; // ! temp: must be overriden by actual mock

@Injectable({
  providedIn: 'root'
})
export class MetricsService {

  constructor(
    private http: HttpClient,
  ) {}

  getWavefrontMetric(params): Observable<any> {
    // TODO: implement mock service instead or returning something different during unit tests.
    if (testController.forTesting) { return of({}); }
    const apiTarget = Apis.metricsWavefront;
    return this.http.get<any>(apiTarget, { params: params }).pipe(
      catchError(e => { console.log(e); return e; })
    );
  }

  getCurrentValueOf(graphData, filter: (seriesData) => boolean, options?) {
    const picked = [];
    for (const seriesData of graphData) {
      if (filter(seriesData)) {
        picked.push(seriesData.data[seriesData.data.length - 1][1]);
      }
    }
    if (!options) {
      options = {
        summarization: 'AVG',
      };
    }
    if (options.summarization === 'AVG') {
      let total = 0;
      for (const value of picked) {
        total += value;
      }
      return total / picked.length;
    }
  }

}
