/*
 * Copyright 2020 VMware, all rights reserved.
 */

import { Component, Input, OnInit, OnChanges, SimpleChanges, ElementRef, ViewChild } from '@angular/core';
import { monotoneX } from './curve.monotone.x.js';
import { HttpClient } from '@angular/common/http';
import { map, catchError } from 'rxjs/operators';
import { Apis } from '../../shared/urls.model.js';

const oneHour = 3600 * 1000;
const oneDay = 24 * oneHour;
const twoDay = 2 * oneDay;
const oneMonth = 30 * oneDay;

const timeStartDefaultAgo = 2 * oneHour;

@Component({
  selector: 'concord-wavefront-graph',
  templateUrl: './wavefront-graph.component.html',
  styleUrls: ['./wavefront-graph.component.scss']
})
export class WavefrontGraphComponent implements OnInit, OnChanges {

  @Input('graphConfig') graphConfig: any;
  @Input('timeStart') timeStart = Date.now() - timeStartDefaultAgo; // UNIX TS, miliseconds
  @Input('timeEnd') timeEnd = Date.now(); // UNIX TS, miliseconds
  @Input('colorScheme') colorScheme = {
    domain: [ // TODO; Default palette, should be improved
      '#FF8A80', '#EA80FC', '#8C9EFF', '#80D8FF', '#67eFbB', '#CCFF90', '#FF9E80',
      '#B37d4e', '#EA80FC', '#8C9EFF', '#80D8FF', '#67eFbB', '#CCFF90', '#FF9E80'
    ]
  };

  @ViewChild('chartContainer', { static: true }) chartContainer: ElementRef;
  @ViewChild('timeSpanPick', { static: true }) timeSpanPick: ElementRef;
  @ViewChild('titleDiv', { static: true }) titleDiv: ElementRef;
  @ViewChild('descriptionDiv', { static: true }) descriptionDiv: ElementRef;

  public graphData;
  public selectedTimeSpan = null;
  public timeStartSaved = Date.now() - timeStartDefaultAgo;
  public timeEndSaved = Date.now();

  // Default options
  public configDefault = {
    title: 'test',
    description: 'test',
    query: { name: '', params: {} },
    showGridLines: true,
    legend: false,
    showLabels: true,
    animations: true,
    xAxis: true,
    yAxis: true,
    showYAxisLabel: false,
    showXAxisLabel: false,
    autoScale: false,
    yScaleMin: null,
    yScaleMax: null,
    xAxisLabel: 'Year',
    yAxisLabel: 'Population',
    timeline: false,
    animation: false,
    curve: monotoneX,
  };

  constructor(
    private http: HttpClient,
  ) { }

  ngOnInit() {
    this.chartContainer.nativeElement.component = this;
  }

  ngOnChanges(changes: SimpleChanges) {
    if (changes.timeEnd) {
      this.timeEndSaved = changes.timeEnd.currentValue;
      if (!this.timeEndSaved) { this.timeEndSaved = Date.now(); }
    }
    if (changes.timeStart) {
      this.timeStartSaved = changes.timeStart.currentValue;
      if (!this.timeStartSaved) { this.timeStartSaved = this.timeEndSaved - timeStartDefaultAgo; }
    }
    if (changes.timeStart || changes.timeEnd) {
      if (this.selectedTimeSpan) {
        this.selectedTimeSpan.classList.remove('timespan-pick-button-selected');
        this.selectedTimeSpan = null;
      }
      if (this.timeSpanPick) {
        this.timeSpanPick.nativeElement.setAttribute('timespan', '');
      }
      setTimeout(() => { this.fetchData(); }, 100);
    }
    if (changes.graphConfig.currentValue) {
      this.loadConfig(changes.graphConfig.currentValue);
    }
  }

  loadConfig(config) {
    for (const configName of Object.keys(this.configDefault)) {
      const configValue = config[configName];
      if (configValue === null || configValue === undefined) {
        config[configName] = this.configDefault[configName];
      }
    }
    this.overrideStyle(this.chartContainer, config.containerStyle);
    this.overrideStyle(this.titleDiv, config.titleStyle);
    this.overrideStyle(this.descriptionDiv, config.descriptionStyle);
    this.fetchData();
  }

  fetchData() {
    const params: any = {
      query_name: this.graphConfig.query.name,
      start: this.timeStart + '',
      end: this.timeEnd + ''
    };
    if (this.graphConfig.query.params.replica_id) {
      params.replica_id = this.graphConfig.query.params.replica_id;
    }
    this.http.get<any>(Apis.metricsWavefront, { params: params}).pipe(
      catchError(e => { console.log(e); return e; }),
      map(data => this.parseWavefrontDataToGraphData(data))
    ).subscribe(data => {
      if (!data || !data.timeseries || data.timeseries.length === 0) { return; }
      this.graphData = data.timeseries;
    });
  }

  dateTickFormatting(val) {
    const xAxisTickElement = this as any;
    if (!xAxisTickElement.chartContainer && xAxisTickElement.ticksElement) {
      let el = xAxisTickElement.ticksElement.nativeElement;
      while ((el = el.parentElement) && !el.classList.contains('chart-container')) {}
      xAxisTickElement.chartContainer = el;
    }
    const _this = (xAxisTickElement.chartContainer && xAxisTickElement.chartContainer.component) ?
                  xAxisTickElement.chartContainer.component : {};
    const diff = _this.timeEnd - _this.timeStart;
    if (val instanceof Date) {
      const d: Date = (<Date>val);
      if (diff < oneDay) {
        return getTimeString(d);
      } else if (diff < twoDay) {
        return getTimeDateString(d);
      } else if (diff <= oneMonth) {
        return getDateString(d);
      } else {
        return getDateYearString(d);
      }
    }
  }

  valueSort(model) {
    return model.sort((a, b) => (b.value - a.value));
  }

  trimValue(v) {
    return parseFloat(v + '').toFixed(2);
  }

  applyToolTipColor(color) {
    return {
      'background-color': color,
      'border': '1px solid ' + color
    };
  }

  timeSpanOverride(span, event) {
    const target = event.target || event.srcElement || event.currentTarget;
    if (this.selectedTimeSpan && this.selectedTimeSpan.span === span) {
      this.timeSpanPick.nativeElement.setAttribute('timespan', '');
      if (this.selectedTimeSpan) { this.selectedTimeSpan.classList.remove('timespan-pick-button-selected'); }
      this.selectedTimeSpan = null;
      this.timeStart = this.timeStartSaved;
      this.timeEnd = this.timeEndSaved;
    } else {
      target.classList.add('timespan-pick-button-selected');
      target.span = span;
      if (this.selectedTimeSpan) { this.selectedTimeSpan.classList.remove('timespan-pick-button-selected'); }
      this.selectedTimeSpan = target;
      this.timeSpanPick.nativeElement.setAttribute('timespan', 'true');
      const now = Date.now();
      this.timeEnd = now;
      switch (span) {
        case '5m': this.timeStart = now - (1 / 12) * oneHour; break;
        case '30m': this.timeStart = now - (1 / 2) * oneHour; break;
        case '2h': this.timeStart = now - 2 * oneHour; break;
        case '1d': this.timeStart = now - 24 * oneHour; break;
        case '8d': this.timeStart = now - 8 * 24 * oneHour; break;
      }
    }
    this.fetchData();
  }

  /** Place holder for later advanced chart event handling */
  onSelect(data): void { data = undefined; return data; }
  onActivate(data): void { data = undefined; return data; }
  onDeactivate(data): void { data = undefined; return data; }

  private parseWavefrontDataToGraphData(data) {
    const selectedTag = this.graphConfig.target.tag;
    if (!data.timeseries) { data.timeseries = []; }
    for (const series of data.timeseries) {
      series.name = series.tags[selectedTag];
      series.series = [];
      for (const datapoint of series.data) {
        series.series.push({
          name: new Date(datapoint[0] * 1000),
          mainTag: series.name,
          value: datapoint[1],
          tags: series.tags
        });
      }
    }
    return data;
  }

  private overrideStyle(elRef, overrideValues) {
    if (elRef && overrideValues) {
      for (const styleName of Object.keys(overrideValues)) {
        const styleValue = overrideValues[styleName];
        elRef.nativeElement.style[styleName] = styleValue;
      }
    }
  }

}


function getTimeString(d: Date) {
  let h: any = d.getHours(); h = h % 12;
  if (h === 0) { h = 12; }
  if (h < 10) { h = '0' + h; }
  let m: any = d.getMinutes(); if (m < 10) { m = '0' + m; }
  const ampm = (d.getHours() >= 12) ? 'PM' : 'AM';
  return h + ':' + m + ' ' + ampm;
}

function getTimeDateString(d: Date) {
  let h: any = d.getHours(); h = h % 12;
  if (h === 0) { h = 12; }
  const ampm = (d.getHours() >= 12) ? 'PM' : 'AM';
  return h + ' ' + ampm + ', ' + d.getMonth() + '/' + d.getDate();
}

function getDateString(d: Date) {
  return d.getMonth() + '/' + d.getDate();
}

function getDateYearString(d: Date) {
  return d.getMonth() + '/' + d.getDate() + '/' + d.getFullYear();
}
