/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, ElementRef, OnInit, Input, Output, EventEmitter, ChangeDetectorRef, ViewChild } from '@angular/core';
import { Validators, FormGroup, FormBuilder } from '@angular/forms';
import { ActivatedRoute } from '@angular/router';
import { TranslateService } from '@ngx-translate/core';

import { ADDRESS_LENGTH, ADDRESS_PATTERN } from '../../shared/shared.config';
import { SmartContractsService } from '../shared/smart-contracts.service';
import { SmartContract, SmartContractCreateResult, SmartContractVersion } from '../shared/smart-contracts.model';
import { newVersionValue } from '../shared/custom-validators';

const addressValidators = [
  Validators.maxLength(ADDRESS_LENGTH),
  Validators.minLength(ADDRESS_LENGTH),
  Validators.pattern(ADDRESS_PATTERN)
];

interface ModalState {
  isUpdate: boolean;
  completed: boolean;
  error: boolean;
  loading: boolean;
  errorMessage?: string;
}

@Component({
  selector: 'athena-contract-form',
  templateUrl: './contract-form.component.html',
  styleUrls: ['./contract-form.component.scss']
})
export class ContractFormComponent implements OnInit {
  @ViewChild('fileInput') fileInput: ElementRef;

  @Input() isOpen = false;
  @Output() contractCreated: EventEmitter<SmartContractCreateResult> = new EventEmitter<SmartContractCreateResult>();

  modalTitle = '';
  smartContractForm: FormGroup;
  readonly modalState: ModalState;

  constructor(
    private formBuilder: FormBuilder,
    private changeDetectorRef: ChangeDetectorRef,
    private smartContractsService: SmartContractsService,
    private route: ActivatedRoute,
    private translate: TranslateService
  ) {
    this.smartContractForm = this.formBuilder.group({
      from: ['', [Validators.required, ...addressValidators]],
      contractId: ['', [Validators.required]],
      version: ['', [Validators.required]],
      file: [null, Validators.required],
    });

    this.modalState = {
      isUpdate: false,
      completed: false,
      error: false,
      loading: false
    };
  }

  ngOnInit() {
    this.route.fragment.subscribe(fragment => {
      switch (fragment) {
        case 'add':
          this.open();
          break;
        default:
          // code...
          break;
      }
    });
  }

  open(smartContract?: SmartContract, version?: SmartContractVersion) {
    this.fileInput.nativeElement.value = '';
    if (smartContract) {
      // Update mode
      this.modalTitle = this.translate.instant('smartContracts.updateContract');
      this.modalState.isUpdate = true;
      this.createUpdateContractForm(smartContract, version);
    } else {
      // create mode
      this.modalTitle = this.translate.instant('smartContracts.createContract');
      this.modalState.isUpdate = false;
      this.createAddContractForm();
    }
    this.isOpen = true;
  }

  onClose() {
    this.isOpen = false;
  }

  onSmartContractFileChange(event) {
    if (event.target.files.length === 0) {
      this.smartContractForm.patchValue({
        file: null
      });
      return;
    }

    const reader = new FileReader();
    reader.onload = () => {
      this.smartContractForm.patchValue({
        file: reader.result
      });
      this.changeDetectorRef.markForCheck();
    };
    reader.readAsText(event.target.files[0]);
  }

  onSubmitSmartContract() {
    this.modalState.error = false;
    this.modalState.completed = false;
    this.modalState.loading = true;
    this.smartContractsService.postContract({
      id: 1,
      from: this.smartContractForm.value.from,
      contract_id: this.smartContractForm.getRawValue().contractId,
      version: this.smartContractForm.value.version,
      sourcecode: this.smartContractForm.value.file
    }).subscribe(
      response => this.handleSmartContract(response),
      response => this.handleError(response.error)
    );
  }

  private createAddContractForm() {
    this.smartContractForm = this.formBuilder.group({
      from: ['', [Validators.required, ...addressValidators]],
      contractId: ['', [Validators.required]],
      version: ['', [Validators.required, Validators.maxLength(16)]],
      file: [null, Validators.required],
    });
  }

  private createUpdateContractForm(smartContract: SmartContract, version: SmartContractVersion) {
    const existingVersions = smartContract.versions.map(x => x.version);
    this.smartContractForm = this.formBuilder.group({
      from: ['', [Validators.required, ...addressValidators]],
      contractId: [smartContract.contract_id, [Validators.required]],
      version: [version.version, [Validators.required, newVersionValue(existingVersions), Validators.maxLength(16)]],
      file: [null, [Validators.required]]
    });
    setTimeout(() => {
      this.smartContractForm.controls['contractId'].disable();
    });
  }

  private handleError(response) {
    this.modalState.loading = false;
    this.modalState.completed = false;
    this.modalState.error = true;
    this.modalState.errorMessage = response.error;
  }

  private handleSmartContract(response) {
    if (response.error) {
      this.handleError(response);
      this.modalState.loading = false;
      this.modalState.completed = false;
      this.modalState.error = true;
      this.modalState.errorMessage = response.error;
    } else {
      this.isOpen = false;
      this.contractCreated.emit(response[0]);
    }
  }
}
