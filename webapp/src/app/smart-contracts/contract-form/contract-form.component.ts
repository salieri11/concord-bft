/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, ElementRef, OnInit, Input, Output, EventEmitter, ChangeDetectorRef, ViewChild } from '@angular/core';
import { Validators, FormGroup, FormBuilder } from '@angular/forms';
import { ActivatedRoute } from '@angular/router';
import { TranslateService } from '@ngx-translate/core';
import { ClrWizard } from '@clr/angular';
import * as Web3EthAbi from 'web3-eth-abi';
import * as Web3Utils from 'web3-utils';

import { ADDRESS_LENGTH, ADDRESS_PATTERN } from '../../shared/shared.config';
import { SmartContractsService } from '../shared/smart-contracts.service';
import {
  SmartContract, SmartContractCreateResult,
  SmartContractVersion, SmartContractCompileResult
} from '../shared/smart-contracts.model';
import { newVersionValue } from '../shared/custom-validators';

const addressValidators = [
  Validators.maxLength(ADDRESS_LENGTH),
  Validators.minLength(ADDRESS_LENGTH),
  Validators.pattern(ADDRESS_PATTERN)
];

interface ModalState {
  isUpdate: boolean;
  isUpdateExternal: boolean;
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
  @ViewChild('wizard') wizard: ClrWizard;
  @ViewChild('fileInput') fileInput: ElementRef;

  @Input() isOpen = false;
  @Output() contractCreated: EventEmitter<SmartContractCreateResult> = new EventEmitter<SmartContractCreateResult>();

  modalTitle = '';
  smartContractForm: FormGroup;
  contractsForm: FormGroup;
  constructorParamsForm: FormGroup;
  multiContractResponse: SmartContractCompileResult[];
  version: SmartContractVersion;
  constructorAbi: any;

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
      file: [null, Validators.required]
    });

    this.contractsForm = this.formBuilder.group({
      selectedContract: ['', [Validators.required]]
    });

    this.constructorParamsForm = new FormGroup({});

    this.modalState = {
      isUpdateExternal: false,
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
    if (version && version.bytecode === '' && version.sourcecode === '') {
      // Update external mode
      this.modalTitle = this.translate.instant('smartContracts.updateContract');
      this.modalState.isUpdateExternal = true;
      this.version = version;
      this.createUpdateExternalContractForm(smartContract, version);
    } else if (smartContract) {
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
    this.reset();
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

  onSubmitSourceCode() {
    this.modalState.error = false;
    this.modalState.completed = false;
    this.modalState.loading = true;
    this.smartContractsService.postSourceCode({
      sourcecode: this.smartContractForm.value.file
    }).subscribe(
      response => this.handleSourceCode(response),
      response => this.handleError(response.error)
    );
  }

  onSelectContract() {
    this.constructorParamsForm = new FormGroup({});
    const selectedContract = this.multiContractResponse.find(item => item.contract_name === this.contractsForm.value.selectedContract);
    this.constructorAbi = selectedContract.metadata.output.abi.find(item => item.type === 'constructor');
    this.wizard.next();
    this.changeDetectorRef.detectChanges();
  }

  onSubmitSmartContract() {
    if (this.modalState.isUpdateExternal) {
      this.updateExistingSmartContract();
    } else {
      this.postSmartContract();
    }
  }

  updateExistingSmartContract() {
    const encodedConstructorParams = this.encodeConstructorParams();

    this.smartContractsService.updateExistingVersion(this.version.contract_id, this.version.version, {
      from: this.smartContractForm.value.from,
      contract_id: this.smartContractForm.getRawValue().contractId,
      version: this.smartContractForm.value.version,
      sourcecode: this.smartContractForm.value.file,
      contractName: this.contractsForm.value.selectedContract,
      constructorParams: encodedConstructorParams,
      existingContractId: this.version.contract_id,
      existingVersionName: this.version.version
    }).subscribe(
      response => this.handleSmartContract(response),
      response => this.handleError(response.error)
    );
  }

  postSmartContract() {
    const encodedConstructorParams = this.encodeConstructorParams();

    this.modalState.error = false;
    this.modalState.completed = false;
    this.modalState.loading = true;
    this.smartContractsService.postContract({
      id: 1,
      from: this.smartContractForm.value.from,
      contract_id: this.smartContractForm.getRawValue().contractId,
      version: this.smartContractForm.value.version,
      sourcecode: this.smartContractForm.value.file,
      contractName: this.contractsForm.value.selectedContract,
      constructorParams: encodedConstructorParams
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

  private createUpdateExternalContractForm(smartContract: SmartContract, version: SmartContractVersion) {
    this.smartContractForm = this.formBuilder.group({
      from: [smartContract.owner, [Validators.required, ...addressValidators]],
      contractId: [smartContract.contract_id, [Validators.required]],
      version: [version.version, [Validators.required]],
      file: [null, [Validators.required]]
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

  private encodeConstructorParams() {
    let encodedParams = '';

    if (this.constructorAbi) {
      const bytesRegex = /^byte[s]?\d{0,2}$/;
      const bytesArrayRegex = /^byte[s]?\d{0,2}\[\d*]$/;
      const paramTypes = this.constructorAbi.inputs.map(input => input.type);
      const paramValues = this.constructorAbi.inputs.map((input) => {
        let value = this.constructorParamsForm.value[input.name];

        if (bytesRegex.test(input.type)) {
          value = Web3Utils.asciiToHex(value);
        } else if (bytesArrayRegex.test(input.type)) {
          value = value.split('\n');
          value = value.map((x) => {
            return Web3Utils.isHexStrict(x) ? x : Web3Utils.asciiToHex(x);
          });
        }
        return value;
      });

      encodedParams = Web3EthAbi.encodeParameters(paramTypes, paramValues).slice(2);
    }

    return encodedParams;
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
      this.contractCreated.emit(response);
      this.reset();

    }
  }

  private handleSourceCode(response) {
    this.contractsForm.controls['selectedContract'].setValue(response.data[0].contract_name);
    this.multiContractResponse = response.data;
    this.wizard.next();
  }

  private reset() {
    this.wizard.reset();
    this.modalState.isUpdateExternal = false;
    this.modalState.isUpdate = false;
    this.modalState.completed = false;
    this.modalState.error = false;
    this.modalState.loading = false;
  }
}
