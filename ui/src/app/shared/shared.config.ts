/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { InjectionToken } from '@angular/core';

export const CONCORD_API_PREFIX = new InjectionToken<string>('ConcordApiPrefix');
export const ETHEREUM_API_PREFIX = new InjectionToken<string>('EthereumApiPrefix');
export const LOG_API_PREFIX = new InjectionToken<string>('LogApiPrefix');
export const CSP_API_PREFIX = new InjectionToken<string>('CspApiPrefix');
export const ADDRESS_LENGTH = 42;
export const ADDRESS_PATTERN = new RegExp(/^(0x|0X)[a-fA-F0-9]+$/);