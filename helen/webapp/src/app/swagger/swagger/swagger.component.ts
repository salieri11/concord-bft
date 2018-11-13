/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { AfterViewInit, Component, ElementRef } from '@angular/core';

// Due to issues with SwaggerUI being undefined in certain scenarios
// https://github.com/swagger-api/swagger-ui/issues/4303
declare var require: any;
const SwaggerUI = require('swagger-ui');

@Component({
  selector: 'athena-swagger',
  templateUrl: './swagger.component.html',
  styleUrls: ['./swagger.component.scss']
})
export class SwaggerComponent implements AfterViewInit {

  constructor(private el: ElementRef) { }

  ngAfterViewInit() {
    const jwt = localStorage.getItem('jwtToken');
    const loginPath = '/auth/login';

    SwaggerUI({
      url: '/assets/static/swagger/swagger.json',
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
        request.headers['Authorization'] = 'Bearer ' + jwt;
        return request;
      },
    });
  }

}
