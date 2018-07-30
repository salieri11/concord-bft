/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit, Input, Output, EventEmitter, ChangeDetectorRef } from '@angular/core';
import { Validators, FormGroup, FormBuilder } from '@angular/forms';

import { ADDRESS_LENGTH, ADDRESS_PATTERN } from '../../shared/shared.config';
import { SmartContractsService } from '../shared/smart-contracts.service';
import { ActivatedRoute } from '@angular/router';

const addressValidators = [
  Validators.maxLength(ADDRESS_LENGTH),
  Validators.minLength(ADDRESS_LENGTH),
  Validators.pattern(ADDRESS_PATTERN)
];

interface ModalState {
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

  @Input() isOpen = false;
  @Output() contractCreated: EventEmitter<void> = new EventEmitter<void>();

  readonly smartContractForm: FormGroup;
  readonly modalState: ModalState;

  constructor(
    private formBuilder: FormBuilder,
    private changeDetectorRef: ChangeDetectorRef,
    private smartContractsService: SmartContractsService,
    private route: ActivatedRoute,
  ) {
    this.smartContractForm = this.formBuilder.group({
      from: ['', [Validators.required, ...addressValidators]],
      contractId: ['', [Validators.required]],
      version: ['', [Validators.required]],
      file: [null, Validators.required],
    });

    this.modalState = {
      completed: false,
      error: false,
      loading: false
    };

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

  ngOnInit() {
  }

  open() {
    this.smartContractForm.reset();
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
      contract_id: this.smartContractForm.value.contractId,
      version: this.smartContractForm.value.version,
      sourcecode: this.smartContractForm.value.file
    }).subscribe(
      response => this.handleSmartContract(response),
      response => this.handleError(response.error)
    );
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
      this.contractCreated.emit();
    }
  }
}
