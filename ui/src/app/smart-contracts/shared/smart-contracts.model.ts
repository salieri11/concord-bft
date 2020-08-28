/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

export interface SmartContract {
  type?: string; // ! Temporary - MR 994, temp indicator of ETH/DAML/HLF
  contract_id: string;
  owner: string;
  url?: string;
  versions?: SmartContractVersionHeader[];
}

export interface SmartContractVersionHeader {
  type?: string; // ! Temporary - MR 994, temp indicator of ETH/DAML/HLF
  address: string;
  metadata: {};
  version: string;
  url: string;
}

export interface SmartContractVersion {
  type?: string; // ! Temporary - MR 994, temp indicator of ETH/DAML/HLF
  contract_id: string;
  version: string;
  owner: string;
  metadata: SmartContractMetadata;
  address: string;
  bytecode?: string;
  sourcecode?: string;
}

export interface SmartContractMetadata {
  compiler: {
    version: string;
  };
  language: string;
  output: {
    abi: AbiDefinition[];
    devdoc: any;
    userdoc: any;
  };
  settings: any;
  sources: any;
  version: number;
}

// TODO: ETH/DAML/HLF ABI might be too different; allocate separate interface/module for each
export interface AbiFunctionDefinition {
  id?: string;
  type: string;
  name?: string;
  constant?: boolean;
  payable?: boolean;
  stateMutability?: string;
  inputs?: AbiFunctionParameter[];
  outputs?: AbiFunctionParameter[];
}

export interface AbiEventDefinition {
  type: string;
  name: string;
  anonymous: boolean;
  inputs: AbiFunctionParameter[];
}

export type AbiDefinition = AbiEventDefinition | AbiFunctionDefinition;

export interface AbiFunctionParameter {
  indexed?: boolean;
  type: string;
  name: string;
}

export interface SmartContractCreateRequest {
  id: number;
  contractId: string;
  version: string;
  from: string;
  sourceCode: string;
}

export interface SmartContractCreateResult {
  contract_id: string;
  version: string;
  url: string;
}

export interface SmartContractCompileResult {
  contract_name: string;
  metadata: SmartContractMetadata;
}
