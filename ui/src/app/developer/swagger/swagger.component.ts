/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { AfterViewInit, Component, ElementRef } from '@angular/core';

import { environment } from './../../../environments/environment';
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
    const jwt = localStorage.getItem('jwtToken');
    let loginPath;
    let apiPath = '/api/static/api.yaml';
    const pathArray = window.location.pathname.split('/');
    pathArray.splice(-3);
    const basePath = pathArray.join('/');
    apiPath = `${basePath}${apiPath}`;
    if (environment.csp) {
      loginPath = environment.loginPath;
    } else {
      loginPath = `${basePath}/auth/login`;
    }

    SwaggerUI({
      url: apiPath,
      domNode: this.el.nativeElement.querySelector('.swagger-container'),
      // deepLinking: true,
      presets: [
        SwaggerUI.presets.apis
      ],
      responseInterceptor: function(response) {
        if (response && response.status === 401) {
          window.location.replace(loginPath);
        }
        return response;
      },
      requestInterceptor: function(request) {
        const location = (window.location as any);
        const url = new URL(request.url, location);
        // Reconstruct the url because the basePath may change
        if (url.pathname.startsWith('/api')) {
          console.log(request);
          const queryParameters = request.url.split('?')[1];
          request.url = `${location.protocol}//${url.host}${basePath}${url.pathname}${queryParameters ? '?' + queryParameters : ''}`;
        }
        request.headers['Authorization'] = 'Bearer ' + jwt;
        return request;
      },
    });
  }

}

export class MockSwaggerComponent implements AfterViewInit {
  ngAfterViewInit() {}
}
