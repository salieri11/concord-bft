/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component } from '@angular/core';
import { Router } from '@angular/router';

import { BlockchainService } from '../../blockchain/shared/blockchain.service';
import { BlockchainResponse } from '../../blockchain/shared/blockchain.model';
import { Personas } from '../../shared/persona.service';
import { External } from '../../shared/urls.model';

export interface WelcomeFlowEvent { action: string; data?: object; }

@Component({
  selector: 'concord-welcome',
  templateUrl: './welcome.component.html',
  styleUrls: ['./welcome.component.scss']
})
export class WelcomeComponent {
  blockchain: BlockchainResponse;
  personas = Personas;
  hasJoinedConsortium = false;
  urls = External;

  constructor(
    private router: Router,
    private blockchainService: BlockchainService
  ) {
    this.hasJoinedConsortium = (this.blockchainService.blockchains && this.blockchainService.blockchains.length > 0);
  }

  deploy() {
    this.router.navigate(this.getValidRoute(), {fragment: 'deploy'});
  }

  go() {
    this.router.navigate(this.getValidRoute(), {fragment: 'orgTour'});
  }

  private getValidRoute() {
    if (this.blockchainService.blockchains && this.blockchainService.blockchains.length > 0) {
      return [this.blockchainService.blockchainId, 'dashboard'];
    } else {
      return [];
    }
  }
}
