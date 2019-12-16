/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, ElementRef, OnInit, Input, Output, EventEmitter, ChangeDetectorRef, ViewChild } from '@angular/core';
import { Validators, FormGroup, FormBuilder } from '@angular/forms';
import { ActivatedRoute } from '@angular/router';
import { TranslateService } from '@ngx-translate/core';
import { ClrWizard } from '@clr/angular';
import * as Web3EthAbi from 'web3-eth-abi';
import * as Web3Utils from 'web3-utils';

import { AuthenticationService } from '../../shared/authentication.service';
import { ADDRESS_LENGTH, ADDRESS_PATTERN } from '../../shared/shared.config';
import { SmartContractsService } from '../shared/smart-contracts.service';
import {
  SmartContract, SmartContractCreateResult,
  SmartContractVersion, SmartContractCompileResult
} from '../shared/smart-contracts.model';
import { newVersionValue } from '../shared/custom-validators';
import { ContextualHelpService } from './../../shared/contextual-help.service';

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
  selector: 'concord-contract-form',
  templateUrl: './contract-form.component.html',
  styleUrls: ['./contract-form.component.scss']
})
export class ContractFormComponent implements OnInit {
  @ViewChild('wizard', { static: true }) wizard: ClrWizard;
  @ViewChild('fileInput', { static: true }) fileInput: ElementRef;

  @Input() isOpen = false;
  @Output() contractCreated: EventEmitter<SmartContractCreateResult> = new EventEmitter<SmartContractCreateResult>();

  modalTitle = '';
  smartContractForm: FormGroup;
  contractsForm: FormGroup;
  constructorParamsForm: FormGroup;
  multiContractResponse: SmartContractCompileResult[];
  version: SmartContractVersion;
  constructorAbi: any;
  walletAddress: string;
  compilerVersions: string[];
  runsDisabled: boolean = true;
  // When loadingFlag is set to true, a spinner will appear on the contract wizard UI
  loadingFlag: boolean = false;

  readonly modalState: ModalState;

  constructor(
    private authenticationService: AuthenticationService,
    private formBuilder: FormBuilder,
    private changeDetectorRef: ChangeDetectorRef,
    private smartContractsService: SmartContractsService,
    private route: ActivatedRoute,
    private translate: TranslateService,
    private helpService: ContextualHelpService
  ) {
    this.smartContractForm = this.formBuilder.group({
      from: ['', [Validators.required, ...addressValidators]],
      contractId: ['', [Validators.required]],
      version: ['', [Validators.required]],
      compilerVersion: ['', [Validators.required]],
      isOptimize: [true, [Validators.required]],
      runs: ['200'],
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

    this.walletAddress = this.authenticationService.currentUser.wallet_address;

    this.smartContractsService.getCompilerVersion()
      .subscribe(response => this.compilerVersions = response);
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
    if (version && (Object.keys(version.metadata).length === 0 || Object.values(version.metadata)[0] === 'Could not read metadata') &&
      version.sourcecode === '') {
      // Update external mode
      this.modalTitle = this.translate.instant('smartContracts.verifyContract');
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
    this.loadingFlag = true;
    this.modalState.error = false;
    this.modalState.completed = false;
    this.modalState.loading = true;
    this.smartContractsService.postSourceCode({
      sourcecode: this.smartContractForm.value.file,
      compiler_version: this.smartContractForm.value.compilerVersion,
      is_optimize: this.smartContractForm.value.isOptimize,
      runs: this.smartContractForm.getRawValue().runs
    }).subscribe(
      response => {
        this.handleSourceCode(response);
        this.loadingFlag = false;
      },
      response => this.handleError(response.error)
    );
  }

  onSelectContract() {
    this.constructorParamsForm = new FormGroup({});
    const selectedContract = this.multiContractResponse.find(item => item.contract_name === this.contractsForm.value.selectedContract);
    if (!this.modalState.isUpdateExternal) {
      this.constructorAbi = selectedContract.metadata.output.abi.find(item => item.type === 'constructor');
    }
    this.wizard.next();
    this.changeDetectorRef.detectChanges();
  }

  onSubmitSmartContract() {
    this.loadingFlag = true;
    if (this.modalState.isUpdateExternal) {
      this.updateExistingSmartContract();
    } else {
      this.postSmartContract();
    }
  }

  updateExistingSmartContract() {
    this.smartContractsService.updateExistingVersion(this.version.contract_id, {
      contract_id: this.smartContractForm.value.contractId,
      sourcecode: this.smartContractForm.value.file,
      contract_name: this.contractsForm.value.selectedContract,
      compiler_version: this.smartContractForm.value.compilerVersion,
      is_optimize: this.smartContractForm.value.isOptimize,
      runs: this.smartContractForm.getRawValue().runs
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
      contract_name: this.contractsForm.value.selectedContract,
      constructor_params: encodedConstructorParams,
      compiler_version: this.smartContractForm.value.compilerVersion,
      is_optimize: this.smartContractForm.value.isOptimize,
      runs: this.smartContractForm.getRawValue().runs
    }).subscribe(
      response => this.handleSmartContract(response),
      response => this.handleError(response.error)
    );
  }

  private createAddContractForm() {
    this.smartContractForm = this.formBuilder.group({
      from: [this.walletAddress, [Validators.required, ...addressValidators]],
      contractId: ['', [Validators.required]],
      version: ['', [Validators.required, Validators.maxLength(16)]],
      compilerVersion: ['', [Validators.required]],
      isOptimize: [true, [Validators.required]],
      runs: ['200'],
      file: [null, Validators.required],
    });
  }

  private createUpdateExternalContractForm(smartContract: SmartContract, version: SmartContractVersion) {
    this.smartContractForm = this.formBuilder.group({
      from: [smartContract.owner, [Validators.required, ...addressValidators]],
      contractId: [smartContract.contract_id, [Validators.required]],
      version: [version.version, [Validators.required]],
      compilerVersion: ['', [Validators.required]],
      isOptimize: [true, [Validators.required]],
      runs: ['200'],
      file: [null, [Validators.required]]
    });
    setTimeout(() => {
      this.smartContractForm.controls['version'].disable();
      this.smartContractForm.controls['from'].disable();
    });
  }

  private createUpdateContractForm(smartContract: SmartContract, version: SmartContractVersion) {
    const existingVersions = smartContract.versions.map(x => x.version);
    this.smartContractForm = this.formBuilder.group({
      from: [this.walletAddress, [Validators.required, ...addressValidators]],
      contractId: [smartContract.contract_id, [Validators.required]],
      version: [version.version, [Validators.required, newVersionValue(existingVersions), Validators.maxLength(16)]],
      compilerVersion: ['', [Validators.required]],
      isOptimize: [true, [Validators.required]],
      runs: ['200'],
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
    this.loadingFlag = false;
    this.modalState.loading = false;
    this.modalState.completed = false;
    this.modalState.error = true;
    this.modalState.errorMessage = response.error ? response.error : response.error_message;
  }

  private handleSmartContract(response) {
    this.loadingFlag = false;
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

  public changeOptimizer() {
    if (this.smartContractForm.value.isOptimize) {
      this.smartContractForm.get('runs').enable();
    } else {
      this.smartContractForm.get('runs').disable();
    }
  }

  onClickToHelp(helpId) {
    this.helpService.openHelpPage(helpId);
  }
}
