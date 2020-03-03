/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

export interface LogTaskResponse {
  documentSelfLink: string;
  end: number;
  logQuery: string;
  rows: number;
  start: number;
  taskInfo: { stage: 'STARTED' | 'FINISHED', isDirect: boolean };
  tenantLinks: string[];
  failureMessage?: string;
}

export interface LogTaskCompletedResponse extends LogTaskResponse {
  logQueryResults: any[];
  partialResults: boolean;
  processedResults: any[];
  nextPageLink: string;
  totalMatchedRecordCount: number;
  totalRecordCount: number;
}

export interface LogTaskParams {
  logQuery: string;
  start: number;
  end: number;
  rows?: number;
}

export interface CspRequestTokenResponse {
  access_token: string;
  expires_in: number;
  id_token: string;
  refresh_token: string;
  scope: string;
  token_type: string;
}

export interface LogCountEntry {
  'count(*)': string;
  timestamp: string;
}

export interface LogListEntry {
  appname: string;
  event_type: string;
  hostname: string;
  id: string;
  ingest_timestamp: string;
  log_timestamp: string;
  log_type: string;
  priority: string;
  process: string;
  sddc_id: string;
  source: string;
  symphony_tenant_id: string;
  text: string;
  timestamp: string;
  ver: string;
}

export interface LogTimePeriod {
  title: string;
  value: number;
  interval: number;
  xAxisLabel?: string;
  yAxisLabel?: string;
}

export enum LogLevels {
  info = 'INFO',
  warn = 'WARN',
  error = 'ERROR'
}
