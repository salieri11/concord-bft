/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';

import { ZonesService } from '../shared/zones.service';
import { Personas } from '../../shared/persona.service';

import { Zone, ZoneType } from '../shared/zones.model';

@Component({
  selector: 'concord-zone-list',
  templateUrl: './zone-list.component.html',
  styleUrls: ['./zone-list.component.scss']
})
export class ZoneListComponent implements OnInit {
  selected: Zone[] = [];
  zones: Zone[];
  personas = Personas;
  zoneType = ZoneType;
  loading: boolean;

  constructor(
    private zonesService: ZonesService
  ) { }

  ngOnInit() {
    this.loading = true;
    this.zonesService.getZones().subscribe(zones => {
      this.zones = zones;
      this.loading = false;
    }, () => this.loading = false);
  }
}
