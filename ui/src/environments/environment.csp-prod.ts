/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { DynamicEnvironment } from './dynamic-environment';

class Environment extends DynamicEnvironment {
  production: boolean = true;
  csp: boolean = true;
  cspEnv: string;
  loginPath: string = '/api/oauth/login';
  iamLink: string = 'https://console.cloud.vmware.com/csp/gateway/portal/#/consumer/usermgmt/users';

  helpUrl: 'https://docs.vmware.com/en/VMware-Blockchain/index.html';
  helpTopicUrl: 'https://docs.vmware.com/en/VMware-Blockchain/1.0/context?id=';

  constructor() {
    super();
  }

}

export const environment = new Environment();