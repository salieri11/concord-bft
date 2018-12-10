/*
 * Copyright 2018 VMware, all rights reserved.
 */

/* SystemJS module definition */
declare var module: NodeModule;
interface NodeModule {
  id: string;
}

declare var Web3EthAbi: any;
declare var Web3Utils: any;

// Support for importing any JSON file in to TypeScript
declare module "*.json" {
  const value: any;
  export default value;
}
