/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

// Time constants in milliseconds
export const ONE_SECOND = 1000;
export const THIRTY_SECONDS = ONE_SECOND * 30;
export const ONE_MINUTE = ONE_SECOND * 60;
export const FIVE_MINUTES = ONE_MINUTE * 5;
export const TEN_MINUTES = ONE_MINUTE * 10;
export const FIFTEEN_MINUTES = ONE_MINUTE * 15;
export const ONE_HOUR = ONE_MINUTE * 60;
export const SIX_HOURS = ONE_HOUR * 6;
export const TWELVE_HOURS = ONE_HOUR * 12;
export const ONE_DAY = ONE_HOUR * 24;
export const SEVEN_DAYS = ONE_DAY * 7;
export const THIRTY_DAYS = ONE_DAY * 30;

export const ALL_SERVICES = 'all';

export const DAML_SERVICE_NAMES = [
  'concord',
  'daml_ledger_api',
  'daml_execution_engine',
  'daml_index_db'
];

export const ETHEREUM_SERVICE_NAMES = [
  'concord',
  'ethrpc'
];
