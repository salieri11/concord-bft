/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { ChangeDetectorRef, Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { TranslateService } from '@ngx-translate/core';

import { Consortium } from '../shared/consortium.model';
import { ConsortiumService } from '../shared/consortium.service';
import { Personas } from '../../shared/persona.service';

@Component({
  selector: 'athena-consortium-form',
  templateUrl: './consortium-form.component.html',
  styleUrls: ['./consortium-form.component.scss']
})
export class ConsortiumFormComponent implements OnInit {
  static personasAllowed: Personas[] = [Personas.SystemsAdmin, Personas.ConsortiumAdmin];
  @Input('selected') selected: Array<Consortium>;
  @Output() consortiumAdd: EventEmitter<any> = new EventEmitter<any>();
  @Output() consortiumDelete: EventEmitter<any> = new EventEmitter<any>();

  openModalForm = false;
  modalTitle = '';
  formType: string;
  modalSize = 'md';

  addConsortiumForm: FormGroup;

  get credentialType(): any {
    return this.addConsortiumForm.get('credentialType');
  }

  deleteConsortiumForm: FormGroup;
  credentialOptions: Array<{ name?: string; value: string }> = [
    { value: 'userAuth' },
    { value: 'certificate' },
    { value: 'configFile' },
  ];

  constructor(
    private consortiumService: ConsortiumService,
    private changeDetectorRef: ChangeDetectorRef,
    private fb: FormBuilder,
    private translate: TranslateService
  ) {

    this.gridOptions.getData = () => {
      return this.consortiumService.getFakeData();
      // return this.consortiumService.getList(params);
    };

    this.handleGrid();
  }

  ngOnInit() {
  }

  openAddConsortium(): void {
    this.openModal('add');
  }

  addConsortium(): void {
    const formModel = this.addConsortiumForm.value;

    const consortium: Consortium = {
      name: formModel.name,
      members: formModel.members,
    };

    this.openModalForm = false;

    this.consortiumService.create(consortium)
      .subscribe(response => this.handleAdd(response));
  }

  confirmDeleteConsortium(): void {
    this.openModal('delete');
  }

  deleteConsortium(): void {
    this.selected.forEach(consortium => {
      this.consortiumService.delete(consortium.id)
        .subscribe(response => this.handleDeletion(response));
    });
  }

  onConfigFileChange(event) {
    if (event.target.files.length === 0) {
      this.addConsortiumForm.patchValue({
        configFile: null
      });
      return;
    }

    const reader = new FileReader();
    reader.onload = () => {
      this.addConsortiumForm.patchValue({
        configFile: reader.result
      });
      this.changeDetectorRef.markForCheck();
    };
    reader.readAsText(event.target.files[0]);
  }

  private createAddConsortiumForm() {
    this.addConsortiumForm = this.fb.group({
      name: ['', Validators.required],
      members: [[], Validators.required]
    });
  }

  private openModal(type: string): void {
    this.formType = type;

    switch (type) {
      case 'add':
        this.createAddConsortiumForm();
        this.modalSize = 'md';
        this.translate.get('consortium.addConsortiumForm.title')
          .subscribe(title => this.modalTitle = title);

        break;

      case 'delete':
        this.modalSize = 'sm';
        this.translate.get('consortium.deleteConsortiumForm.title')
          .subscribe(title => this.modalTitle = title);
        break;
    }

    this.openModalForm = true;
  }

  private handleAdd(response): void {
    console.log('add response', response);
    this.openModalForm = false;
    this.consortiumAdd.emit(response);
  }

  private handleDeletion(response): void {
    console.log('deletion response', response);
    this.openModalForm = false;
    this.consortiumDelete.emit(response);
  }
}
