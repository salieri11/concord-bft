/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

/**
 * GET response of fetching a list of blocks
 */
export interface BlockListing {
  blocks: BlockInfo[];
  next: string;
}

/**
 * A simplified block provided when fetching a list of blocks
 */
export interface BlockInfo {
  number: number;
  hash: string;
  url: string;
  // TODO: Make Helen also fetch transaction count and timestamp of this block
  blockData?: Block;
}

/**
 * GET response of fetching an individual block
 */
export interface Block {
  hash: string;
  nonce: string;
  parentHash: string;
  number: number;
  size: number;
  timestamp: number;
  transactions: [{hash: string; url: string; }];
}
