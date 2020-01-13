/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';
import { FormGroup, FormControl, Validators } from '@angular/forms';

import { BlockchainService } from '../../blockchain/shared/blockchain.service';
import { NodesService } from '../shared/nodes.service';
import { Zone } from '../../zones/shared/zones.model';

@Component({
  selector: 'concord-deploy-client',
  templateUrl: './deploy-client.component.html',
  styleUrls: ['./deploy-client.component.scss']
})
export class DeployClientComponent implements OnInit {
  isOpen: boolean;
  zones: Zone[];
  deployClient = new FormGroup({
    zone: new FormControl('', Validators.required)
  });

  constructor(
    private blockchainService: BlockchainService,
    private nodesService: NodesService,
  ) {
    this.zones = this.blockchainService.zones;
  }

  ngOnInit() {}

  deploy() {
    const selectedZoneId = this.deployClient.controls['zone'].value;
    const selectedZone = this.zones.find(zone => zone.id === selectedZoneId);

    this.nodesService.deployClients([selectedZoneId], selectedZone.name);

    this.deployClient.reset();
    this.closeModal();
  }

  closeModal() {
    this.isOpen = false;
  }

  openModal() {
    this.zones = this.blockchainService.zones;
    this.isOpen = true;
  }


}
