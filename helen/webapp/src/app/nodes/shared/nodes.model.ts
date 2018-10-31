/*
 * Copyright 2018 VMware, all rights reserved.
 */

/**
 * GET response of fetching a list of members
 */
export interface Node {
  hostname: string;
  status: string;
  address: string;
  millis_since_last_message: number;
  millis_since_last_message_threshold: number;
}
