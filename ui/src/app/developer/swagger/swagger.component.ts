/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { AfterViewInit, Component, ElementRef } from '@angular/core';

import { environment } from './../../../environments/environment';
import { Apis } from '../../shared/urls.model';
import { testController } from '../../../test.controller'; // ! temp: must be overriden by actual mock

// Due to issues with SwaggerUI being undefined in certain scenarios
// https://github.com/swagger-api/swagger-ui/issues/4303
declare var require: any;
const SwaggerUI = require('swagger-ui');

@Component({
  selector: 'concord-swagger',
  templateUrl: './swagger.component.html',
  styleUrls: ['./swagger.component.scss']
})
export class SwaggerComponent implements AfterViewInit {

  constructor(private el: ElementRef) { }

  ngAfterViewInit() {
    try {
      // During unit tests it cannot fetch api.yaml; useless.
      // Swagger will be checked by e2e.
      if (testController.forTesting) { return; }
      const jwt = localStorage.getItem('jwtToken');
      let loginPath;
      let apiPath = Apis.swaggerYAML;
      const pathArray = window.location.pathname.split('/');
      pathArray.splice(-3);
      const basePath = pathArray.join('/');
      apiPath = `${basePath}${apiPath}`;
      if (environment.csp) {
        loginPath = environment.loginPath;
      }

      SwaggerUI({
        url: apiPath,
        domNode: this.el.nativeElement.querySelector('.swagger-container'),
        // deepLinking: true,
        presets: [
          SwaggerUI.presets.apis
        ],
        responseInterceptor: function(response) {
          try {
            if (response && response.status === 401) {
              window.location.replace(loginPath);
            }
            return response;
          } catch (e) { console.log(e); }
        },
        requestInterceptor: function(request) {
          try {
            const location = (window.location as any);
            const url = new URL(request.url, location);
            // Reconstruct the url because the basePath may change
            if (url.pathname.startsWith(Apis.base)) {
              const queryParameters = request.url.split('?')[1];
              request.url = `${location.protocol}//${url.host}${basePath}${url.pathname}${queryParameters ? '?' + queryParameters : ''}`;
            }
            request.headers['Authorization'] = 'Bearer ' + jwt;
          return request;
          } catch (e) { console.log(e); }
        },
      });

    } catch (e) {console.log(e); }
  }

}
