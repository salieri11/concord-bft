/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { FormControl, FormGroup, Validators } from '@angular/forms';
import {
  Component,
  Input,
  OnChanges,
  SimpleChanges,
  ViewChild,
  OnInit,
  OnDestroy,
} from '@angular/core';
import { TranslateService } from '@ngx-translate/core';
import { ActivatedRoute } from '@angular/router';

import { SmartContractVersion, AbiFunctionDefinition } from '../shared/smart-contracts.model';

import { AuthenticationService } from '../../shared/authentication.service';
import { EthApiService } from '../../shared/eth-api.service';
import { isHexAddress } from '../shared/custom-validators';
import { generateDownload } from '../../shared/download-helpers';
import { ContractPayloadPreviewFormComponent } from '../contract-payload-preview-form/contract-payload-preview-form.component';
import { TourService } from '../../shared/tour.service';
import { ContractEngines } from '../../blockchain/shared/blockchain.model';
import { damlFlattenStruct, damlExtractDataFromDar, DamlDarExtractionType } from '../../shared/daml.utils';
import { RouteService } from '../../shared/route.service';
import { Subscription, Observable } from 'rxjs';
import { HttpClient } from '@angular/common/http';
import { BlockchainService } from '../../blockchain/shared/blockchain.service';
import { delay } from 'rxjs/operators';
import { IndexedDBService } from '../../shared/indexeddb.service';
import { DamlDarAvailableActions } from '../../app-intercept.daml.proxy';
import { FeatureFlags, FeatureFlagService } from '../../shared/feature-flag.service';

import * as Web3Utils from 'web3-utils';
declare var require: any;
const abiCoder = require('web3-eth-abi');

interface DamlDisplayData {

  disableDarMetadataCaching?: boolean; // Save parsed to indexedDB?

  // States
  extracting?: boolean; // DAML DAR Extraction
  extractProgress?: number;
  extractMessage?: string;
  extractFilename?: string;

  // DAML source files tree view
  sourceFileActive?;
  sourceHandleActive?;
  sourceTree?: Observable<any>;
  sourceList?: any[];
  sourceTreeData?: any[];
  sourceAdd: (source) => void;
  sourceGetChildren: (folder) => Observable<any>;

  selectedTemplate?: string;
  selectedTemplateDef?: AbiFunctionDefinition;
  selectedModuleName?: string;
  selectedEntityName?: string;

  contractStructMap?: any;
}

interface DamlDarCacheUnzipped {
  data: string;
  metadata: string;
}

@Component({
  selector: 'concord-smart-contract-version',
  templateUrl: './smart-contract-version.component.html',
  styleUrls: ['./smart-contract-version.component.scss']
})
export class SmartContractVersionComponent implements OnChanges, OnInit, OnDestroy {

  @Input() version: SmartContractVersion;
  @ViewChild('payloadPreviewModal', { static: true }) payloadPreviewModal: ContractPayloadPreviewFormComponent;

  functions;
  functionDefinition;

  versionForm: FormGroup;
  inputs = [];
  outputs = [];
  alertMessage: string;
  alertType: string;
  resultType: string;
  metadataString: string;
  callReturnValue: string;
  rawCallReturnValue: string;
  walletAddress: string;

  routeFragmentSub: Subscription;
  linkActionSub: Subscription;

  parseIncomplete?: boolean = false;

  // Engine type class, for quick reference in HTML template
  isETH: boolean;
  isDAML: boolean;
  isHLF: boolean;

  daml: DamlDisplayData = {
    disableDarMetadataCaching: false,
    sourceTree: null,
    sourceList: [],
    sourceTreeData: [],
    sourceGetChildren: damlSourceGetChildren,
    sourceAdd: damlSourceAdd
  };

  get damlExplorerEnabled() {
    return this.featureFlagService.check(FeatureFlags.daml_contracts_explorer) ? true : false;
  }

  constructor(
    private authenticationService: AuthenticationService,
    private ethApiService: EthApiService,
    private translate: TranslateService,
    private route: ActivatedRoute,
    private tourService: TourService,
    private routeService: RouteService,
    private http: HttpClient,
    private blockchainService: BlockchainService,
    private indexedDBService: IndexedDBService,
    private featureFlagService: FeatureFlagService,
  ) {
    this.versionForm = new FormGroup({
      functionName: new FormControl(''),
      contractForm: new FormGroup({
        from: new FormControl('', [Validators.required, isHexAddress]),
        functionInputs: new FormGroup({})
      })
    });
    this.walletAddress = this.authenticationService.currentUser.wallet_address;

    if (this.damlExplorerEnabled) {
      this.linkActionSub = this.routeService.linkAction.subscribe(link => {
        if (link.action === DamlDarAvailableActions.extractDar) {
          if (link.params.contract_id === this.version.contract_id
              && link.params.contract_version === this.version.version) {
            this.damlLoadDarMetadata();
          }
        }
      });
    }
  }

  ngOnInit() {
    this.routeFragmentSub = this.route.fragment.subscribe(fragment => {
      switch (fragment) {
        case 'new':
          this.tourService.startContractTour();
          break;
        default:
          // code...
          break;
      }
    });
  }

  ngOnDestroy() {
    if (this.routeFragmentSub) { this.routeFragmentSub.unsubscribe(); }
    if (this.linkActionSub) { this.linkActionSub.unsubscribe(); }
  }

  ngOnChanges(changes: SimpleChanges) {
    if (changes.version) {
      this.onVersionChange(changes.version.currentValue);
    }
  }

  getFunctionDetails() {
    (this.versionForm.get('contractForm') as FormGroup).setControl('functionInputs', new FormGroup({}));
    this.versionForm.get('contractForm').reset({from: this.walletAddress});
    const result = this.functions.filter(func => func.name === this.versionForm.value.functionName);
    if (result.length > 0) {
      this.inputs = result[0].inputs;
      this.outputs = result[0].outputs;
      this.functionDefinition = result[0];
    } else {
      this.inputs = [];
      this.functionDefinition = null;
    }
  }

  onSourceCodeDownload() {
    generateDownload(`${this.generateFileName()}_source_code.sol`, this.version.sourcecode);
  }

  onByteCodeDownload() {
    generateDownload(`${this.generateFileName()}_bytecode.bin`, this.version.bytecode);
  }

  onMetadataDownload() {
    generateDownload(`${this.generateFileName()}_metadata.json`, JSON.stringify(this.version.metadata, null, 4));
  }

  onPreview() {
    this.payloadPreviewModal.open(JSON.stringify(this.encodeFunction(), null, 4));
  }

  onCall() {
    this.ethApiService.sendCall(this.encodeFunction()).subscribe((resp) => {
      if (resp.error) {
        this.handleError(resp);
      } else {
        this.alertMessage = this.translate.instant('smartContracts.form.callSuccessMessage');
        this.alertType = 'alert-success';
        this.resultType = 'call';
        this.rawCallReturnValue = resp.result;
        const decodedValues = abiCoder.decodeParameters(this.outputs, resp.result);
        delete decodedValues.__length__;
        this.callReturnValue = JSON.stringify(decodedValues, null, 4);
      }
    }, errorResp => this.handleError(errorResp));
  }

  onSend() {
    this.ethApiService.sendTransaction(this.encodeFunction())
    .subscribe((resp) => {
      if (resp.error) {
        this.handleError(resp);
      } else {
        this.alertMessage = resp.result;
        this.alertType = 'alert-success';
        this.resultType = 'send';
      }
    }, errorResp => this.handleError(errorResp));
  }

  sizeFormat(bytes: number, decimals = 2): string {
    if (bytes === 0) { return '0 Bytes'; }
    const k = 1024;
    const dm = decimals < 0 ? 0 : decimals;
    const sizes = ['Bytes', 'KB', 'MB', 'GB', 'TB', 'PB', 'EB'];
    const i = Math.floor(Math.log(bytes) / Math.log(k));
    return parseFloat((bytes / Math.pow(k, i)).toFixed(dm)) + ' ' + sizes[i];
  }

  onVersionChange(version: SmartContractVersion) {
    this.isETH = (this.blockchainService.type === ContractEngines.ETH);
    this.isDAML = (this.blockchainService.type === ContractEngines.DAML);
    this.isHLF = (this.blockchainService.type === ContractEngines.HLF);
    if (this.damlExplorerEnabled) { this.isETH = this.isHLF = false; this.isDAML = true; }

    if (!version.metadata || Object.keys(version.metadata).length === 0) { return; }
    const metadata = version.metadata;

    if (this.isETH) {
      this.functions = metadata.output.abi.filter(abi => abi.type === 'function').sort((a, b) => {
        const aName = a.name.toUpperCase();
        const bName = b.name.toUpperCase();
        return (aName < bName) ? -1 : (aName > bName) ? 1 : 0;
      });
      this.versionForm.reset();
      this.versionForm.value.contractForm.functionInputs = new FormGroup({});
      if (this.functions.length) {
        this.functionDefinition = this.functions[0];
        this.versionForm.patchValue({ functionName: this.functionDefinition.name });
      } else { this.functionDefinition = undefined; }
      this.getFunctionDetails();
      this.highlightCode();

    } else if (this.isDAML) {
      setTimeout(async () => {
        const fetchPath = this.damlGetDarFilePath();
        const cached = await this.indexedDBService.get<DamlDarCacheUnzipped>(fetchPath);
        if (!this.daml.disableDarMetadataCaching && cached) {
          this.daml.extracting = false;
          const allFiles = JSON.parse(cached.data);
          this.daml.sourceTree = new Observable<any>(a => { a.next(allFiles.tree); });
          this.version.metadata = allFiles.metadata;
          this.version.sourcecode = ' ';
          this.parseIncomplete = false;
          this.damlReflectMetadata();
        } else {
          const damlMetadata = metadata as any;
          if (damlMetadata.size < 1024 * 1024) {
            await this.damlLoadDarMetadata(true);
          } else {
            this.parseIncomplete = true;
            this.damlReflectMetadata();
          }
        }
      });
    } else {
      console.log('Unknonw Contract Engine Type');
    }
  }

  damlGetDarFilePath() {
    // TODO: Helen needs to implement downloading of DAR file after deploying DAR package
    return `/api/blockchains/${this.blockchainService.blockchainId}`
          + `/concord/contracts/${this.version.contract_id}`
          + `/versions/${this.version.version}/file`;
  }

  damlTemplateSelect(funcName: string) {
    if (this.daml.selectedTemplate === funcName) { return; }
    const funcFound = this.functions.filter(func => (func.name === funcName))[0];
    if (!funcFound) { return; }
    this.daml.selectedTemplateDef = null;
    setTimeout(() => {
      const split = funcName.split('.');
      this.daml.selectedEntityName = split.pop();
      this.daml.selectedModuleName = split.join('.');
      this.daml.selectedTemplate = funcName;
      this.daml.selectedTemplateDef = funcFound;
    });
  }

  damlOnFileHandleClick(e, file) {
    e.stopPropagation();
    if (this.daml.sourceHandleActive) { this.daml.sourceHandleActive.handleActive = false; }
    this.daml.sourceHandleActive = file;
    this.daml.sourceHandleActive.handleActive = true;
    if (file.isFolder) {
      file.expanded = !file.expanded;
    } else {
      if (this.daml.sourceFileActive) { this.daml.sourceFileActive.active = false; }
      this.daml.sourceFileActive = file;
      this.daml.sourceFileActive.active = true;
      this.version.sourcecode = file.content;
      this.highlightCode();
    }
  }

  damlLoadDarMetadata(instant: boolean = false): Promise<any> {
    return new Promise(async resolve => {
      let initialMetadata, sources = [], count = 0, total = 0;
      if (!instant) { this.daml.extracting = true; }
      this.daml.sourceTreeData = [];
      this.daml.sourceList = [];
      this.daml.extractProgress = 0;
      this.daml.extractMessage = 'Loading DAR Package...';
      const fetchPath = this.damlGetDarFilePath();
      this.http.get<Blob>(fetchPath)
        .pipe(delay(instant ? 0 : 1000)).subscribe((response: Blob) => {
          damlExtractDataFromDar(response, instant).subscribe(async e => {
            switch (e.type) {
              case DamlDarExtractionType.unzipStart:
                this.daml.extractMessage = 'Inflating DAR Package...';
                this.daml.extractProgress = 10;
              break;
              case DamlDarExtractionType.unzipEnd:
                this.daml.extractMessage = 'Extracting Meta Information...';
                this.daml.extractProgress = 20;
              break;
              case DamlDarExtractionType.metaInf:
                initialMetadata = e.data;
                this.daml.extractProgress = 30;
                this.daml.sourceAdd({
                  name: '/META-INF',
                  content: JSON.stringify(e.data, null, 4)
                });
              break;
              case DamlDarExtractionType.sourceFile:
                total = initialMetadata['file-count'].daml;
                const value = Math.round(60 * (++count) / total);
                let name = e.data.name.split('/'); name.shift(); name = name.join('.');
                this.daml.extractMessage = `Extracting DAML Source Code File (${count}/${total})`;
                this.daml.extractFilename = name;
                this.daml.extractProgress = value + 30;
                if (count === total) {
                  this.daml.extractFilename = '';
                  this.daml.extractMessage = 'Mapping Function Interfaces...';
                }
                this.daml.sourceAdd(e.data);
              break;
              case DamlDarExtractionType.sourceAll:
                this.daml.sourceTree = new Observable<any>(a => { a.next(this.daml.sourceTreeData); });
                sources = e.data;
              break;
              case DamlDarExtractionType.abi:
                this.daml.extractProgress = 95;
              break;
              case DamlDarExtractionType.finished:
                this.indexedDBService.put({ _id: fetchPath, data: JSON.stringify({
                  list: this.daml.sourceList,
                  tree: this.daml.sourceTreeData,
                  metadata: e.data
                }) });
                this.daml.extractProgress = 100;
                this.daml.extractMessage = 'Finished';
                if (!instant) { setTimeout(() => { this.daml.extracting = false; }, 1000); }
                this.version.metadata = e.data;
                if (sources.length > 2) { sources.length = 2; }
                this.version.sourcecode = sources.join('\n');
                this.parseIncomplete = false;
                this.damlReflectMetadata();
                resolve({ metadata: e.data, sourcecode: sources.join('\n') });
              break;
            }
        });
      });
    });
  }

  private encodeFunction() {
    const bytesRegex = /^byte[s]?\d{0,2}$/;
    const bytesArrayRegex = /^byte[s]?\d{0,2}\[\d*]$/;
    const paramsForm = this.versionForm.get('contractForm').get('functionInputs');
    const params = this.inputs.map((input) => {
      let value = paramsForm.value[input.name];
      if (!value) { value = ''; } // package no longer takes null value for encoding
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

    let output = '';
    try { output = abiCoder.encodeFunctionCall(this.functionDefinition, params); } catch (e) {}

    return {
      from: this.versionForm.value.contractForm.from,
      to: this.version.address,
      gas: '0xF4240',
      data: output
    };
  }

  private generateFileName() {
    return `${this.version.contract_id}_${this.version.version}`;
  }

  private handleError(error) {
    this.alertMessage = error.error.message ? error.error.message : error.error;
    this.alertType = 'alert-danger';
    this.resultType = 'error';
  }

  private highlightCode() {
    const jsonStr = JSON.stringify(this.version.metadata, null, 4);
    this.metadataString = jsonStr.substr(0, 1024 * 16);
    // TODO: At over 16KB+ JSON string, UI starts to lag severely, handle this gracefully
  }

  private damlReflectMetadata() {
    const abi: AbiFunctionDefinition[] = this.version.metadata.output.abi;
    this.daml.contractStructMap = {};
    for (const funcDef of abi) { this.daml.contractStructMap[funcDef.name] = funcDef; }
    const abiCopy = JSON.parse(JSON.stringify(abi));
    for (const funcDef of abiCopy) { funcDef.inputs = damlFlattenStruct(funcDef.inputs, this.daml.contractStructMap); }
    this.functions = abiCopy;
    if (this.functions.length > 0) { this.damlTemplateSelect(this.functions[0].name); }
    this.highlightCode();
  }

}


function damlSourceGetChildren(folder) {
  if (!folder.isFolder) { return null; }
  return new Observable<any>(ob => {
    if (!folder.isOrdered) {
      folder.files = folder.files.sort((a, b) => {
        if (a.isFolder && b.isFolder) { return a.name.localeCompare(b.name); }
        if (a.isFolder) { return -1; } if (b.isFolder) { return 1; }
        return a.name.localeCompare(b.name);
      });
      folder.isOrdered = true;
    }
    ob.next(folder.files);
  });
}

function damlSourceAdd(data) {
  const filepaths = data.name.split('/'); filepaths.shift();
  let list = this.sourceTreeData;
  while (filepaths.length > 1) {
    const folderName = filepaths.shift();
    let folder = list.filter(file => file.name === folderName)[0];
    if (!folder) { folder = { name: folderName, isFolder: true, files: [] }; list.push(folder); }
    list = folder.files;
  }
  list.push({ name: filepaths[0], content: data.content });
  this.sourceList.push({ name: data.name, content: data.content });
}

