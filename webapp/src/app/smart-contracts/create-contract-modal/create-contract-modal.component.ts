/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit, Input, Output, EventEmitter, ChangeDetectorRef } from '@angular/core';
import { ADDRESS_LENGTH, ADDRESS_PATTERN } from '../../shared/shared.config';
import { Validators, FormGroup, FormBuilder } from '@angular/forms';
import { AthenaApiService } from '../../shared/athena-api.service';

const addressValidators = [
  Validators.maxLength(ADDRESS_LENGTH),
  Validators.minLength(ADDRESS_LENGTH),
  Validators.pattern(ADDRESS_PATTERN)
];

@Component({
  selector: 'app-create-contract-modal',
  templateUrl: './create-contract-modal.component.html',
  styleUrls: ['./create-contract-modal.component.scss']
})
export class CreateContractModalComponent implements OnInit {

  @Input() isOpen = false;
  @Output() contractCreated: EventEmitter<void> = new EventEmitter<void>();

  readonly smartContractForm: FormGroup;
  readonly modalState: ModalState;

  constructor(private formBuilder: FormBuilder,
              private changeDetectorRef: ChangeDetectorRef,
              private athenaApiService: AthenaApiService) {
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
  }

  ngOnInit() {
  }

  open(open) {
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
    this.athenaApiService.postContract({
      id: 1,
      from: this.smartContractForm.value.from,
      contract_id: this.smartContractForm.value.contractId,
      version: this.smartContractForm.value.version,
      sourcecode: this.smartContractForm.value.file
    }).subscribe(response => {
      if (response.error) {
        this.handleError(response);
        this.modalState.loading = false;
        this.modalState.completed = false;
        this.modalState.error = true;
        this.modalState.errorMessage = response.error.message;
      } else {
        this.isOpen = false;
        this.contractCreated.emit();
      }
    }, response => {
      this.handleError(response.error);
    });
  }

  handleError(response) {
    this.modalState.loading = false;
    this.modalState.completed = false;
    this.modalState.error = true;
    this.modalState.errorMessage = response.error.message;
  }

}

interface ModalState {
  completed: boolean;
  error: boolean;
  loading: boolean;
  errorMessage?: string;
}
