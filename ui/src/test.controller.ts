/*
 * Copyright 2018-2020 VMware, all rights reserved.
 */
declare var require: any;
export const testController = { forTesting: false };
const SwaggerToothAngular = require('swaggertooth/angular.js').SwaggerToothAngular;

// From https://www.npmjs.com/package/swaggertooth
export class SwaggerMocksController {

  generated: boolean = false;
  info; // JSON object with generation mapping info
  infoPath: string; // where mapping info file is
  servePath: string;
  mocksConfig = require('../swagger.mocks.conf.json');
  swaggertooth;

  constructor() {
    this.servePath = this.mocksConfig['angularServePath'];
    this.swaggertooth = new SwaggerToothAngular(this.mocksConfig);
    this.infoPath = this.servePath + '/generation.info.json';
  }

  async sampleResponse(methodAndPath: string, statusCode: number = 200): Promise<any> {
    return this.swaggertooth.sampleResponse(methodAndPath, statusCode) as Promise<any>;
  }

  async sampleRequest(methodAndPath: string): Promise<any> {
    return this.swaggertooth.sampleRequest(methodAndPath) as Promise<any>;
  }
}

export const swaggerMocks = new SwaggerMocksController();

