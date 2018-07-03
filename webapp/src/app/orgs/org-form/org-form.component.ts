/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { ChangeDetectorRef, Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { ActivatedRoute } from '@angular/router';
import { TranslateService } from '@ngx-translate/core';

import { Org } from '../shared/org.model';
import { OrgService } from '../shared/org.service';
import { ErrorAlertService } from '../../shared/global-error-handler.service';
import { Personas } from '../../shared/persona.service';

@Component({
  selector: 'athena-orgs-form',
  templateUrl: './org-form.component.html',
  styleUrls: ['./org-form.component.scss']
})
export class OrgFormComponent implements OnInit {
  static personasAllowed: Personas[] = [Personas.SystemsAdmin, Personas.ConsortiumAdmin];
  @Input('selected') selected: Array<Org>;
  @Output('addOrganization') addOrganization: EventEmitter<any> = new EventEmitter<any>();
  @Output('deleteOrganizations') deleteOrganizations: EventEmitter<any> = new EventEmitter<any>();

  openModalForm = false;
  modalTitle = '';
  formType: string;
  modalSize = 'md';

  addOrgForm: FormGroup;
  addingOrg = false;
  importOrgForm: FormGroup;
  deleteOrgForm: FormGroup;
  orgTypes: Array<string> = ['peer', 'orderer'];

  constructor(
    private orgService: OrgService,
    private fb: FormBuilder,
    private changeDetectorRef: ChangeDetectorRef,
    private translate: TranslateService,
    private route: ActivatedRoute,
    private errorService: ErrorAlertService
  )  { }

  ngOnInit() {
    this.route.fragment.subscribe(fragment => {
        switch (fragment) {
          case 'add':
            this.openAddOrg();
            break;

          default:
            // code...
            break;
        }
    });
  }

  openAddOrg(): void {
    this.openModal('add');
  }

  addOrg(): void {
    this.addingOrg = true;
    const formModel = this.addOrgForm.value;

    const org: Org = {
      name: formModel.name,
      peerNumber: formModel.peerNumber,
      type: formModel.type,
      domain: formModel.domain
    };

    this.orgService.create(org)
      .subscribe(
        response => this.handleAdd(response),
        error => this.handleError(error),
      );
  }

  openImportForm(): void {
    this.openModal('import');
  }

  onPrivateKeyChange(event) {
    if (event.target.files.length === 0) {
      this.importOrgForm.patchValue({
        privateKey: null
      });
      return;
    }
    const reader = new FileReader();
    reader.onload = () => {
      this.importOrgForm.patchValue({
        privateKey: reader.result
      });
      this.changeDetectorRef.markForCheck();
    };
    reader.readAsText(event.target.files[0]);
  }

  importOrg(): void {
    const formModel = this.importOrgForm.value;

    const orgImport = {
      name: formModel.name,
      certificate: formModel.certificate,
      privateKey: formModel.privateKey,
    };

    this.orgService.import(orgImport)
      .subscribe(
        response => this.handleImport(response),
      );
  }

  onCertifcateChange(event) {
    if (event.target.files.length === 0) {
      this.importOrgForm.patchValue({
        certificate: null
      });
      return;
    }

    const reader = new FileReader();
    reader.onload = () => {
      this.importOrgForm.patchValue({
        certificate: reader.result
      });
      this.changeDetectorRef.markForCheck();
    };
    reader.readAsText(event.target.files[0]);
  }

  confirmDeleteOrg(): void {
    this.openModal('delete');
  }

  deleteOrg(): void {
    this.selected.forEach(org => {
      this.orgService.delete(org.id)
        .subscribe(
          response => this.handleDeletion(response),
          error => this.handleError(error),
        );
    });
  }

  private createAddOrgForm() {
    this.addOrgForm = this.fb.group({
      name: ['', Validators.required],
      peerNumber: ['', Validators.required],
      domain: ['', Validators.required],
      type: ['', Validators.required]
    });
  }

  private createImportOrgForm() {
    this.importOrgForm = this.fb.group({
      name: ['', Validators.required],
      certificate: [null, Validators.required],
      privateKey: [null, Validators.required]
    });
  }

  private openModal(type: string): void {
    this.formType = type;

    switch (type) {
      case 'add':
        this.createAddOrgForm();
        this.modalSize = 'md';
        this.translate.get('organization.addOrgForm.title')
          .subscribe(title => this.modalTitle = title);

        break;

      case 'import':
        this.modalSize = 'md';
        this.translate.get('organization.importOrgForm.title')
          .subscribe(title => this.modalTitle = title);
        this.createImportOrgForm();

        break;

      case 'delete':
        this.modalSize = 'sm';
        this.translate.get('organization.deleteOrgForm.title')
          .subscribe(title => this.modalTitle = title);
        break;
    }

    this.openModalForm = true;

  }

  private handleAdd(response): void {
    console.log('add response', response);
    this.addingOrg = false;
    this.openModalForm = false;
    this.addOrganization.emit(response);
  }

  private handleImport(response): void {
    console.log('import response', response);
    this.openModalForm = false;
  }

  private handleDeletion(response): void {
    console.log('deletion response', response);
    this.openModalForm = false;
    this.deleteOrganizations.emit();
  }

  private handleError(error): void {
    console.log('handle error', error);
    this.addingOrg = false;
    this.addingOrg = false;
    this.openModalForm = false;
    this.errorService.add(error);
  }
}
