/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, Params } from '@angular/router';

import { BlockchainsService } from '../shared/blockchains.service';
import { Blockchain } from '../shared/blockchains.model';
import { Personas } from '../../shared/persona.service';

@Component({
  selector: 'app-blockchain',
  templateUrl: './blockchain.component.html',
  styleUrls: ['./blockchain.component.scss']
})
export class BlockchainComponent implements OnInit {
  static personasAllowed: string[] = [Personas.SystemsAdmin, Personas.ConsortiumAdmin];
  blockchain: Blockchain = new Blockchain();
  orgUrl: string;
  channelUrl: string;
  peerUrl: string;

  constructor(
    private route: ActivatedRoute,
    private blockchainService: BlockchainsService,
  ) { }

  ngOnInit() {
    this.route.params
      .subscribe(params => this.handleRoutes(params));
  }

  private handleRoutes(params: Params): void {
    if (params['id']) {
      this.blockchainService.get(params['id'])
        .subscribe(blockchain => this.handleBlockchain(blockchain));
    }
  }

  private handleBlockchain(blockchain: Blockchain): void {
    this.blockchain = blockchain;

    if (blockchain._links.organizations) {
      this.orgUrl = blockchain._links.organizations.href;
    }

  }

}
