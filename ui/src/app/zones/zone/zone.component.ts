/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, OnInit, ViewChild } from '@angular/core';
import { Location } from '@angular/common';
import { ActivatedRoute, Router } from '@angular/router';
import { TranslateService } from '@ngx-translate/core';

import { Personas } from '../../shared/persona.service';
import { mainRoutes } from '../../shared/urls.model';
import { ConfirmModalComponent } from '../../shared/components/confirm-modal/confirm-modal.component';

import { ZonesService } from '../shared/zones.service';
import { VmwTasksService } from '../../shared/components/task-panel/tasks.service';
import { ZoneFormComponent } from '../zone-form/zone-form.component';
import { VmwToastType } from '@vmw/ngx-components';

@Component({
  selector: 'concord-zone',
  templateUrl: './zone.component.html',
  styleUrls: ['./zone.component.scss']
})
export class ZoneComponent implements OnInit {
  @ViewChild('form', { static: true }) zoneForm: ZoneFormComponent;
  @ViewChild('confirm', { static: true }) confirm: ConfirmModalComponent;
  zoneId: string;
  deleting: boolean;
  saving: boolean;
  zoneName: string;
  isNewZoneState: boolean;

  personas = Personas;

  constructor(
    private loc: Location,
    private route: ActivatedRoute,
    private zoneService: ZonesService,
    private router: Router,
    private translate: TranslateService,
    private taskService: VmwTasksService
  ) { }

  ngOnInit() {
    this.route.params.subscribe(params => {
      this.zoneId = params['zoneId'];
      if (this.zoneId === mainRoutes.new) {
        this.isNewZoneState = true;
      } else {
        this.isNewZoneState = false;
        this.zoneService.getZone(this.zoneId).subscribe(zone => this.setZone(zone));
      }
    });
  }

  update() {
    this.saving = true;
    this.zoneForm.update(this.zoneId).subscribe(zone => {
      this.saving = false;
      this.setZone(zone);

      this.taskService.addToast({
        title: this.translate.instant('zones.actions.updated'),
        description: `${this.translate.instant('zones.titleDetail')} ${this.zoneName}`,
        type: VmwToastType.INFO,
      });

    }, () => this.saving = false);
  }

  openConfirmDelete() {
    this.confirm.openModal(
      this,
      'delete',
      this.translate.instant('confirmModal.delete'),
      this.translate.instant('common.delete'),
    );
  }

  delete() {
    this.deleting = true;
    this.zoneService.delete(this.zoneId).subscribe(() => {
      const path = this.loc.path().split('/');
      path.pop();
      this.router.navigate([path.join('/')]);
      this.deleting = false;
    }, () => this.deleting = false);
  }

  addZone() {
    this.saving = true;
    this.zoneForm.addOnPrem().subscribe(zone => {
      const path = this.loc.path().split('/');
      path.pop();
      this.router.navigate([path.join('/'), zone.id]);
      this.saving = false;
      this.taskService.addToast({
        title: this.translate.instant('zones.actions.added'),
        description: `${this.translate.instant('zones.titleDetail')} ${zone.name}`,
        type: VmwToastType.INFO,
      });

    }, () => this.saving = false);
  }

  private setZone(zone) {
    const zoneNameList = zone.name.split(' - ');
    const zoneName = [{
      displayValue: zoneNameList[0], value: zoneNameList[0],
      geometry: {
        long: zone.longitude,
        lat: zone.latitude
      }
    }];
    const zoneDesignation = zoneNameList[1];
    this.zoneName = zone.name;

    this.zoneForm.form.controls.onPrem.patchValue(zone);
    this.zoneForm.form.controls.onPremLocation.patchValue({ name: zoneDesignation, location: zoneName });
    this.zoneForm.form.patchValue(zone);

    if (zone.container_repo) {
      this.zoneForm.form.controls.container_repo.patchValue(zone.container_repo);
    }
    if (zone.wavefront) {
      this.zoneForm.form.controls.wavefront.patchValue(zone.wavefront);
    }
    if (zone.log_insight) {
      this.zoneForm.form.controls.log_insight.patchValue(zone.log_insight);
    }
    if (zone.outbound_proxy) {
      this.zoneForm.form.controls.outbound_proxy.patchValue(zone.outbound_proxy);
    }
  }
}
