/*
 * Copyright 2019 VMware, all rights reserved.
 */

import { Component } from '@angular/core';
import { CspNoServiceAccessPageMode } from '@vmw/csp-ngx-components';

import { environment } from './../../../environments/environment';
import { AuthenticationService } from './../../shared/authentication.service';


@Component({
  selector: 'concord-forbidden',
  templateUrl: './forbidden.component.html',
  styleUrls: ['./forbidden.component.scss']
})
export class ForbiddenComponent {
  env = environment;
  orgId: string;
  refLink: string;
  mode = CspNoServiceAccessPageMode.NORMAL;

  constructor(private authService: AuthenticationService) {
    const parsedToken = this.authService.parsedToken;
    this.orgId = parsedToken.context_name;
    this.refLink = `/csp/gateway/slc/api/definitions/external/${parsedToken.context}`;
  }

}
