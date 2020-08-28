/*
 * Copyright 2018-2019 VMware, all rights reserved.
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

/**
 * GET response of fetching a list of transactions
 */
export interface TransactionListing {
  transactions: Transaction[];
  next: string;
}
