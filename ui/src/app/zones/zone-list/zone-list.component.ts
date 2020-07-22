/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, ViewChild } from '@angular/core';

import { ZonesService } from '../shared/zones.service';
import { Personas } from '../../shared/persona.service';

import { Zone, ZoneType } from '../shared/zones.model';
import { ContextualHelpService } from './../../shared/contextual-help.service';
import { Router, ActivatedRoute } from '@angular/router';
import { TranslateService } from '@ngx-translate/core';
import { VmwTasksService } from '../../shared/components/task-panel/tasks.service';
import { ConfirmModalComponent } from '../../shared/components/confirm-modal/confirm-modal.component';
import { VmwToastType } from '@vmw/ngx-components';
import { BlockchainService } from '../../blockchain/shared/blockchain.service';

@Component({
  selector: 'concord-zone-list',
  templateUrl: './zone-list.component.html',
  styleUrls: ['./zone-list.component.scss']
})
export class ZoneListComponent {
  @ViewChild('confirm', { static: true }) confirm: ConfirmModalComponent;
  selected: Zone[] = [];
  zones: Zone[];
  targetZone: Zone;
  personas = Personas;
  zoneType = ZoneType;
  loading: boolean;

  constructor(
    private zonesService: ZonesService,
    private helpService: ContextualHelpService,
    private translate: TranslateService,
    private router: Router,
    private route: ActivatedRoute,
    private taskService: VmwTasksService,
    private blockchainService: BlockchainService,
  ) {
    this.zones = this.blockchainService.zones;
    this.refreshList();
  }

  refreshList() {
    this.loading = true;
    this.blockchainService.getZones().subscribe(() => {
      this.zones = this.blockchainService.zones;
      this.loading = false;
    });
  }

  onClickToHelp(helpId) {
    this.helpService.openHelpPage(helpId);
  }

  editZone(zone: Zone) {
    this.router.navigate([`../${zone.id}`], { relativeTo: this.route });
  }

  confirmDelete(zone: Zone) {
    this.targetZone = zone;
    setTimeout(() => {
      this.confirm.openModal(
        this,
        'deleteZone',
        this.translate.instant('zones.actions.deleteAsk')
                      .replace('TARGET_ZONE', this.targetZone.name),
        this.translate.instant('common.delete'),
      );
    }, 100);
  }

  deleteZone() {
    if (!this.targetZone) { return; }
    this.zonesService.delete(this.targetZone.id).subscribe(() => {
      this.refreshList();
      this.confirm.open = false;
      this.taskService.addToast({
        title: this.translate.instant('zones.actions.deleted'),
        description: `${this.translate.instant('zones.actions.deleteMsg')} ${this.targetZone.name}`,
        type: VmwToastType.SUCCESS,
      });
    });
  }

}
