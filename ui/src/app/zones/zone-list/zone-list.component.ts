/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, ElementRef, ViewChild } from '@angular/core';

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
import { map, catchError } from 'rxjs/operators';


interface FileReaderEventTarget extends EventTarget {
  result: string;
}

interface FileReaderEvent extends Event {
  target: FileReaderEventTarget;
  getMessage(): string;
}

interface HTMLInputEvent extends Event {
  target: HTMLInputElement & EventTarget;
}

@Component({
  selector: 'concord-zone-list',
  templateUrl: './zone-list.component.html',
  styleUrls: ['./zone-list.component.scss']
})
export class ZoneListComponent {
  @ViewChild('confirm', { static: true }) confirm: ConfirmModalComponent;
  @ViewChild('fileUpload', { static: true }) fileUpload: ElementRef;
  selected: Zone[] = [];
  zones: Zone[];
  targetZone: Zone;
  personas = Personas;
  zoneType = ZoneType;
  loading: boolean;
  uploading: boolean;

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

  uploadZone() {
    const fileUpload = this.fileUpload.nativeElement;
    fileUpload.click();

    fileUpload.onchange = (e: HTMLInputEvent) => {
      const reader = new FileReader();
      reader.onload = this.onReaderLoad.bind(this);
      reader.readAsText(e.target.files[0]);
      this.uploading = true;
    };
  }

  private onReaderLoad(event: FileReaderEvent) {
    const zone = JSON.parse(event.target.result);
    // Clean out ID
    delete zone.id;

    this.zonesService.addZone(zone).pipe(
      map(response => this.handleSave(response)),
      // @ts-ignore
      catchError<OnPremZone>(error => this.handleError(error))
    ).subscribe();
  }

  private handleSave(response) {
    this.uploading = false;

    this.taskService.addToast({
      title: this.translate.instant('zones.actions.added'),
      description: `${this.translate.instant('zones.titleDetail')} ${response.name}`,
      type: VmwToastType.SUCCESS,
    });

    this.blockchainService.getZones().subscribe();
    this.router.navigate([response.id], { relativeTo: this.route });
  }

  private handleError(error) {
    this.uploading = false;
    this.taskService.addToast({
      title: this.translate.instant('zones.actions.failedUpload'),
      description: `${this.translate.instant('zones.actions.failedUploadDesc')} ${error.name}`,
      type: VmwToastType.FAILURE,
    });
  }
}
