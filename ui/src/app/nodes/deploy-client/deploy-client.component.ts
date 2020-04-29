/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';
import { FormGroup, FormControl, Validators } from '@angular/forms';

import { BlockchainService } from '../../blockchain/shared/blockchain.service';
import { NodesService } from '../shared/nodes.service';
import { Zone } from '../../zones/shared/zones.model';

// Works for domains and ips
const urlValidateRegex = /^(?:http(s)?:\/\/)[\w.-]+(?:\.[\w\.-]+)+[\w\-\._~:/?#[\]@!\$&'\(\)\*\+,;=.]+$/;

@Component({
  selector: 'concord-deploy-client',
  templateUrl: './deploy-client.component.html',
  styleUrls: ['./deploy-client.component.scss']
})
export class DeployClientComponent implements OnInit {
  isOpen: boolean;
  zones: Zone[];
  deployClient = new FormGroup({
    name: new FormControl('', Validators.required),
    zone: new FormControl('', Validators.required),
    high_availability: new FormControl(''), // high availability
    auth_url: new FormControl('', [Validators.pattern(urlValidateRegex)]),
  });

  constructor(
    private blockchainService: BlockchainService,
    private nodesService: NodesService,
  ) {
    this.setZones();
  }

  ngOnInit() {}

  deploy() {
    const selectedZoneId = this.deployClient.controls['zone'].value;
    // const selectedZone = this.zones.find(zone => zone.id === selectedZoneId);
    const clientJwt = this.deployClient.get('auth_url').value;
    const deployParams = {
      zone_ids: [selectedZoneId],
      client_jwt: clientJwt
    };

    this.nodesService.deployClients(deployParams);

    this.deployClient.reset();
    this.closeModal();
  }

  closeModal() {
    this.isOpen = false;
  }

  openModal() {
    this.setZones();
    this.isOpen = true;
  }

  private setZones() {

    if (this.blockchainService.zones) {
      // Don't fileter based on zone type (See BC-2286 number 2)
      this.zones = this.blockchainService.zones;
      // const isOnPremZone = this.blockchainService.zones.some(zone => zone.type === ZoneType.ON_PREM);
      // if (isOnPremZone) {
      //   const onPremZones = this.blockchainService.zones.filter((zone) => zone.type === ZoneType.ON_PREM);
      //   this.zones = onPremZones;
      // } else {
      //   this.zones = this.blockchainService.zones;
      // }
    }
  }

}
