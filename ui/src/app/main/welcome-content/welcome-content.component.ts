/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component } from '@angular/core';
import { Router } from '@angular/router';

import { BlockchainService } from '../../blockchain/shared/blockchain.service';
import { BlockchainResponse } from '../../blockchain/shared/blockchain.model';
import { Personas } from '../../shared/persona.service';
import { External, mainRoutes, mainFragments } from '../../shared/urls.model';
import { RouteService } from '../../shared/route.service';

export interface WelcomeFlowEvent { action: string; data?: object; }

@Component({
  selector: 'concord-welcome-content',
  templateUrl: './welcome-content.component.html',
  styleUrls: ['./welcome-content.component.scss']
})
export class WelcomeContentComponent {
  blockchain: BlockchainResponse;
  personas = Personas;
  hasJoinedConsortium = false;
  urls = External;

  constructor(
    private router: Router,
    private blockchainService: BlockchainService,
    private routeService: RouteService
  ) {
    this.hasJoinedConsortium = !this.blockchainService.noConsortiumJoined;
  }

  deploy() {
    this.router.navigate([mainRoutes.blockchain, mainRoutes.deploy]);
  }

  async go() {
    await this.routeService.resolveConsortium();
    this.routeService.redirectToDefault(mainFragments.defaultTour);
  }

}
