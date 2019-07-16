/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { DynamicEnvironment } from './dynamic-environment';

class Environment extends DynamicEnvironment {
  production: boolean = false;
  csp: boolean = true;
  cspEnv: string;
  loginPath: string = '/api/oauth/login';

  constructor() {
    super();
  }

}

export const environment = new Environment();
