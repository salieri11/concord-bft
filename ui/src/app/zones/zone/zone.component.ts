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
import { VmwClarityThemeService, VmwClarityTheme } from '../../shared/theme.provider';
import { OnPremZone, ZoneType } from '../shared/zones.model';

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
  zoneFormVisible: boolean = false;
  themeDark: boolean = false;
  canDelete: boolean = false;
  zoneData: OnPremZone;

  personas = Personas;

  constructor(
    private loc: Location,
    private zoneService: ZonesService,
    private route: ActivatedRoute,
    private router: Router,
    private translate: TranslateService,
    private taskService: VmwTasksService,
    private themeService: VmwClarityThemeService,
  ) {
    this.themeService.themeChange.subscribe(theme => {
      this.themeDark = theme === VmwClarityTheme.Dark;
    });
  }

  ngOnInit() {
    this.route.params.subscribe(params => {
      this.zoneId = params['zoneId'];
      this.zoneService.canDelete(this.zoneId).subscribe(canDelete => this.canDelete = canDelete);

      if (this.zoneId === mainRoutes.new) { // New Zone Form
        this.isNewZoneState = true;
        this.zoneFormVisible = true;
      } else { // UUID; Edit Existing Zone
        this.isNewZoneState = false;
        this.zoneService.getZone(this.zoneId).subscribe(async zone => {
          // Cloud zones are forbidden to edit, go back to list if trying to access cloud zone.
          if (zone.type === ZoneType.VMC_AWS) {
            this.router.navigate(['../'],  { relativeTo: this.route });
            return;
          }
          this.zoneData = zone;
          this.setZone(zone);
          await this.zoneForm.afterLoadingFormForUpdate();
          this.zoneFormVisible = true;
        }, e => {
          if (e && e.error && e.error.status === 404) {
            // Zone not found; go back to list
            this.router.navigate(['../'],  { relativeTo: this.route });
          }
        });
      }
    });
  }

  update() {
    this.saving = true;
    this.zoneForm.update(this.zoneId).subscribe(zone => {
      this.setZone(zone);
      this.saving = false;
      this.taskService.addToast({
        title: this.translate.instant('zones.actions.updated'),
        description: `${this.translate.instant('zones.titleDetail')} ${this.zoneName}`,
        type: VmwToastType.SUCCESS,
      });
    });
  }

  openConfirmDelete() {
    this.confirm.openModal(
      this,
      'deleteZone',
      this.translate.instant('zones.actions.deleteAsk')
                    .replace('TARGET_ZONE', this.zoneData.name),
      this.translate.instant('common.delete'),
    );
  }

  deleteZone() {
    this.deleting = true;
    this.zoneService.delete(this.zoneId).subscribe(() => {
      const name = this.zoneData.name;

      this.router.navigate(['../'], { relativeTo: this.route });
      this.deleting = false;
      this.taskService.addToast({
        title: this.translate.instant('zones.actions.deleted'),
        description: `${this.translate.instant('zones.actions.deleteMsg')} ${name}`,
        type: VmwToastType.SUCCESS,
      });
    });
  }

  addZone() {
    this.saving = true;
    this.zoneForm.addOnPrem().subscribe(zone => {
      const path = this.loc.path().split('/');
      path.pop();
      this.router.navigate([path.join('/')]); // Go to zones list
      this.saving = false;
      this.taskService.addToast({
        title: this.translate.instant('zones.actions.added'),
        description: `${this.translate.instant('zones.titleDetail')} ${zone.name}`,
        type: VmwToastType.SUCCESS,
      });
    });
  }

  downloadZoneConfig() {
    const dataStr = 'data:text/json;charset=utf-8,' + encodeURIComponent(JSON.stringify(this.zoneData, null, 2));
    const downloadAnchorNode = document.createElement('a');
    const name = this.zoneData.name.split(' ').join('_');
    downloadAnchorNode.setAttribute('href', dataStr);
    downloadAnchorNode.setAttribute('download', `vmbc-zone-on-prem--${name}.json`);
    document.body.appendChild(downloadAnchorNode); // required for firefox
    downloadAnchorNode.click();
    downloadAnchorNode.remove();
  }

  private setZone(zone) {
    this.zoneForm.settingZone = true;
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

    if (zone.log_managements) {
      this.zoneForm.form.controls.log_managements.patchValue(zone.log_managements);
    }
    if (zone.wavefront) {
      this.zoneForm.form.get('metrics').get('wavefront').patchValue(zone.wavefront);
    }
    if (zone.elasticsearch) {
      this.zoneForm.form.get('metrics').get('elasticsearch').patchValue(zone.elasticsearch);
    }
    if (zone.container_repo) {
      this.zoneForm.form.controls.container_repo.patchValue(zone.container_repo);
    }
    if (zone.outbound_proxy && this.allKeysHaveValue(zone.outbound_proxy)) {
      this.zoneForm.form.controls.outbound_proxy.patchValue(zone.outbound_proxy);
    }

    // Prevent multiple testing of the same credentials by letting know what's already been tested
    this.zoneForm.originalValue = JSON.stringify(this.zoneForm.getEffectiveZoneData());
    this.zoneForm.changedFromOriginalValue = false;
    this.zoneForm.onPremConnectionSuccessful = true;
    this.zoneForm.onPremConnectionLastTested = this.zoneForm.getVCenterCredentialIndex().index;
    this.zoneForm.settingZone = false;
  }

  private allKeysHaveValue(obj: object, skip: string[] = []) {
    let result = true;
    for (const key of Object.keys(obj)) {
      if (skip.indexOf(key) >= 0) { continue; }
      if (!obj[key]) { result = false; break; }
    }
    return result;
  }
}

