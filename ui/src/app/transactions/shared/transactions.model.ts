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

export const mockTransactionDefault: Transaction = {
  input: '0x00000000',
  block_hash: '0xb00000000000000000000000000000000000000000000000000000000000000',
  block_number: 3,
  from: '0x0000000000000000000000000000000000000001',
  to: '0x0000000000000000000000000000000000000002',
  value: '0x0',
  nonce: 1,
  hash: '0xa000000000000000000000000000000000000000000000000000000000000000',
  status: 1
};

export const mockTransactions: Transaction[] = [
  mockTransactionDefault
];
