/*
 * Copyright 2018 VMware, all rights reserved.
 */

/**
 * GET response of fetching an individual transaction
 */
export interface Transaction {
  hash: string;
  from: string;
  to?: string;
  contract_address?: string;
  value: string;
  input: string;
  nonce: number;
  status: number;
  block_hash: string;
  block_number: number;
}
