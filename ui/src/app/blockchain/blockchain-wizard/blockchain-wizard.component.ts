/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import {
  Component,
  AfterViewInit,
  ViewChild,
  Output,
  EventEmitter,
  ElementRef
} from '@angular/core';
import { FormControl, FormGroup, Validators, ValidatorFn, ValidationErrors, FormArray } from '@angular/forms';
import { Router } from '@angular/router';
import { ClrWizard, ClrWizardPage } from '@clr/angular';

import { PersonaService } from '../../shared/persona.service';

import { BlockchainService } from '../shared/blockchain.service';
import { AuthenticationService } from '../../shared/authentication.service';
import { BlockchainRequestParams, ContractEngines } from '../shared/blockchain.model';
import { mainRoutes } from '../../shared/urls.model';
import { NodeTemplateFormResponse, NodeClientParam } from '../../nodes/shared/nodes.model';
import { Zone, ZoneType } from '../../zones/shared/zones.model';
import { ZoneFormComponent } from '../../zones/zone-form/zone-form.component';
import { RouteService } from '../../shared/route.service';
import { ContextualHelpService } from './../../shared/contextual-help.service';
import { TranslateService } from '@ngx-translate/core';


const urlValidateRegex = /^(?:http(s)?:\/\/)[\w.-]+(?:\.[\w\.-]+)+[\w\-\._~:/?#[\]@!\$&'\(\)\*\+,;=.]+$/;

const RegionCountValidator: ValidatorFn = (fg: FormGroup): ValidationErrors | null => {
  const nodes = fg['controls'].numberOfNodes.value;
  const zones = fg['controls'].zones;
  let count = 0;

  Object.keys(zones.value).forEach(key => {
    count = count + Number(zones.value[key]);
  });

  return nodes === count ? { 'countIsCorrect': true } : { 'countIsCorrect': false };
};

interface EventData { type: string; data?: any; error?: Error; }


@Component({
  selector: 'concord-blockchain-wizard',
  templateUrl: './blockchain-wizard.component.html',
  styleUrls: ['./blockchain-wizard.component.scss']
})
export class BlockchainWizardComponent implements AfterViewInit {
  @ViewChild('wizard', { static: false }) wizard: ClrWizard;
  @ViewChild('detailPage', { static: false }) detailPage: ClrWizardPage;
  @ViewChild('usersPage', { static: false }) usersPage: ClrWizardPage;
  @ViewChild('onPremPage', { static: false }) onPremPage: ClrWizardPage;
  @ViewChild('replicaPage', { static: false }) replicaPage: ClrWizardPage;
  @ViewChild('clientsPage', { static: false }) clientsPage: ClrWizardPage;
  @ViewChild('onPremForm', { static: false }) onPremForm: ZoneFormComponent;
  @ViewChild('consortiumInput', { static: false }) consortiumInput: ElementRef;
  @ViewChild('clientsForm', { static: false }) clientsForm: ElementRef;

  @Output('events') events: EventEmitter<EventData> = new EventEmitter<EventData>();

  engines = ContractEngines;
  zoneType = ZoneType;

  selectedEngine: string;
  selectedZoneType: ZoneType;
  isOpen = false;
  form: FormGroup;

  personaOptions = PersonaService.getOptions();
  numbersOfNodes = [4, 7];
  fCountMapping = { '4': 1, '7': 2 };
  zones: Zone[] = [];
  onPremZones: Zone[] = [];
  cloudZones: Zone[] = [];

  showOnPrem: boolean;
  loadingFlag: boolean;

  onPremActive: boolean;
  cloudActive: boolean;
  hasOnPrem: boolean = true;
  zonesSetUp: boolean = false;

  clientGroups: FormArray;
  maxClients = 10;
  maxClientsPerGroup = 5;
  alwaysGroup = true;

  pastContractEngineStep: boolean = false;
  sizingIsValid: boolean;

  nodeSizingTemplate: NodeTemplateFormResponse;

  submitting: boolean;
  submitErrorMessage: string;
  submitError: boolean;

  constructor(
    private blockchainService: BlockchainService,
    private authService: AuthenticationService,
    private routeService: RouteService,
    private router: Router,
    private helpService: ContextualHelpService,
    private translate: TranslateService,
  ) {
    if (!this.isAuthorized()) { this.router.navigate([mainRoutes.forbidden]); }
    this.form = this.initForm();
    this.onPremActive = this.hasOnPrem = this.blockchainService.zones.some(zone => zone.type === ZoneType.ON_PREM);
  }

  ngAfterViewInit() {
    this.wizard.currentPageChanged.subscribe(() => this.handlePageChange());
  }

  selectEngine(type: string) {
    this.selectedEngine = type;
    if (this.selectedEngine !== 'DAML') {
      (this.form.get('clients') as FormArray).clear();
    }
  }

  onClickNext() {
    this.pastContractEngineStep = true;
  }

  onCancel() {
    this.events.emit({ type: 'cancel' });
  }

  personaName(value): string {
    return PersonaService.getName(value);
  }

  open() {
    this.isOpen = true;
    this.showOnPrem = false;
    this.selectedEngine = undefined;
    this.submitErrorMessage = undefined;

    if (this.form && this.wizard) {
      this.wizard.reset();
      this.form.reset();
    }
  }

  zoneTabSelect(zoneType: ZoneType) {
    this.selectedZoneType = zoneType;
    this.setupZones(true);
  }

  // Clients & Group functions
  addClientGroup(groupName: string = '') {
    const g = this.clientGroups.length;
    this.clientGroups.push(new FormGroup({
      group_name: new FormControl(groupName,
        [Validators.required, this.groupNameUniqueValidator.bind(this)]
      ),
      member_clients: new FormArray([]),
    }));
    this.addClientNodeToGroup(g);
    // Focus group name input
    setTimeout(() => {
      this.clientsForm
        .nativeElement
        .querySelector('#deploy-page-clients-group-' + g + '-name-input').focus();
    }, 500);

  }

  getClientGroup(g: number) { return this.clientGroups.at(g); }

  removeClientGroup(g: number) { this.clientGroups.removeAt(g); }

  getClientGroupMembers(g: number) { return this.getClientGroup(g)['controls'].member_clients; }

  addClientNodeToGroup(g: number) {
    const defaultZoneId = this.getActiveZones().length === 1 ? this.getActiveZones()[0].id : '';
    this.getClientGroupMembers(g).push(
      new FormGroup({
        name: new FormControl(''),
        zone_id: new FormControl(defaultZoneId, Validators.required),
        auth_url_jwt: new FormControl('', Validators.pattern(urlValidateRegex)),
        zone_name: new FormControl(''),
        pem: new FormControl(''),
        crt: new FormControl(''),
        cacrt: new FormControl(''),
      })
    );
  }

  removeClientNodeToGroup(g: number, c: number) {
    if (this.getClientGroupMembers(g).length > 1) {
      this.getClientGroupMembers(g).removeAt(c);
    } else {
      this.removeClientGroup(g);
    }
  }

  addTLSCert(c: number) {
    const client = document.querySelector(`#Client-${c}`);
    if (client.classList.contains('hide')) {
      client.classList.remove('hide');
    } else {
      client.classList.add('hide');
    }
  }

  clientNodeIndex(g: number, c: number) {
    let total = 0;
    for (let g2 = 0; g2 < this.clientGroups.length; ++g2) {
      if (g === g2) { break; }
      total += this.getClientGroupMembers(g2).length;
    }
    return total + c;
  }

  totalClientsCount() {
    let total = 0;
    for (let g = 0; g < this.clientGroups.length; ++g) {
      total += this.getClientGroupMembers(g).length;
    }
    return total;
  }

  clientNodesValidator(fa: FormArray): ValidationErrors | null {
    let currentClientIndex = 0;
    const errors = [];
    const clientGroups = fa;
    const clientNodeI18N = this.translate.instant('blockchainWizard.clients.clientNode');
    const groupNameErrorI18N = this.translate.instant('blockchainWizard.clients.groupNameError');
    const zoneSelectErrorI18N = this.translate.instant('blockchainWizard.clients.zoneSelectError');
    const authUrlErrorI18N = this.translate.instant('blockchainWizard.clients.authUrlError');
    for (let g = 0; g < clientGroups.length; ++g) {
      const clientGroup = clientGroups.at(g) as FormGroup;
      const groupName = clientGroup.controls.group_name as FormControl;
      const memberClients = clientGroup.controls.member_clients as FormArray;
      if ((this.alwaysGroup || memberClients.length > 1) && !groupName.valid) {
        errors.push(`${clientNodeI18N} ${currentClientIndex + 1} :: ${groupNameErrorI18N}`);
      }
      for (let c = 0; c < memberClients.length; ++c) {
        const clientNode = memberClients.at(c) as FormGroup;
        const zoneId = clientNode.controls.zone_id as FormControl;
        if (zoneId.value === '') {
          errors.push(`${clientNodeI18N} ${currentClientIndex + 1} :: ${zoneSelectErrorI18N}`);
        }
        const authUrl = clientNode.controls.auth_url_jwt as FormControl;
        if (!authUrl.valid) {
          errors.push(`${clientNodeI18N} ${currentClientIndex + 1} :: ${authUrlErrorI18N}`);
        }
        ++currentClientIndex;
      }
    }
    return { messages: errors };
  }

  groupNameUniqueValidator(fc: FormControl): ValidationErrors | null  {
    const groupName = fc.value;
    if (!groupName) { return null; }
    for (let g = 0; g < this.clientGroups.length; ++g) {
      const clientGroup = this.clientGroups.at(g) as FormGroup;
      const groupNameFC = clientGroup.controls.group_name as FormControl;
      if (groupNameFC === fc) { continue; }
      if (groupNameFC.value === groupName) {
        const msg = this.translate.instant('blockchainWizard.clients.groupNameErrorDupe')
                                        .replace('{{groupName}}', groupName);
        return { message: msg };
      }
    }
    return null;
  }

  private setupZones(tabMoved?: boolean) {
    if (tabMoved || !this.zonesSetUp) {
      this.zonesSetUp = true;
      this.hasOnPrem = this.blockchainService.zones.some(zone => zone.type === ZoneType.ON_PREM);
      const onPremZones = this.onPremZones = this.blockchainService.zones.filter((zone) => zone.type === ZoneType.ON_PREM);
      const cloudZones = this.cloudZones = this.blockchainService.zones.filter((zone) => zone.type === ZoneType.VMC_AWS);
      if (tabMoved) { this.clientGroups.clear(); }
      if (this.form) {
        const zones = this.form['controls'].nodes['controls'].zones;
        const pastZones = this.zones;
        this.zones = [];
        pastZones.forEach(zone => { zones.removeControl(zone.id); });
        if (this.onPremActive) {
          if (!tabMoved) { this.selectedZoneType = ZoneType.ON_PREM; }
          onPremZones.forEach(zone => {
            zones.addControl(zone.id, new FormControl(''));
          });
          this.zones = onPremZones;
        } else {
          if (!tabMoved) { this.selectedZoneType = ZoneType.VMC_AWS; }
          this.cloudActive = true;
          cloudZones.forEach(zone => {
            zones.addControl(zone.id, new FormControl(''));
          });
          this.zones = cloudZones;
        }
      }
      this.distributeZones();
    }
  }

  onSubmit() {
    this.submitting = true;
    const params = new BlockchainRequestParams();
    params.consortium_name = this.form.value.details.consortium_name;
    params.blockchain_type = this.selectedEngine;

    // Handle zones info
    const zones = this.form.controls.nodes['controls'].zones.value;
    const isOnlyOnPrem = this.zones.some(zone => zone.type === ZoneType.ON_PREM);
    let zoneIds = [];
    Object.keys(zones).forEach(zoneId => zoneIds = zoneIds.concat(Array(Number(zones[zoneId])).fill(zoneId)));

    // Node Sizing for replica nodes
    if (this.selectedEngine === ContractEngines.DAML) {
      params.replica_nodes = [];
      const committerSizing = this.nodeSizingTemplate.committerSizing;

      for (const zone of zoneIds) {
        params.replica_nodes.push({
          zone_id: zone,
          sizing_info: {
            no_of_cpus: committerSizing.no_of_cpus,
            storage_in_gigs: committerSizing.storage_in_gigs,
            memory_in_gigs: committerSizing.memory_in_gigs
          }
        });
      }
    } else {
      params.replica_zone_ids = zoneIds;
    }

    // Handle client nodes
    const clients: NodeClientParam[] = [];
    for (let g = 0; g < this.clientGroups.length; ++g) {
      const clientGroup = this.clientGroups.at(g) as FormGroup;
      const groupName = clientGroup.controls.group_name.value;
      const memberClients = clientGroup.controls.member_clients as FormArray;
      for (let c = 0; c < memberClients.length; ++c) {
        const clientNode = memberClients.at(c).value;
        const clientSizing = this.nodeSizingTemplate.clientSizing;

        clients.push({
          // should be groupName.value in the future
          group_name: groupName,
          zone_id: clientNode.zone_id,
          auth_url_jwt: clientNode.authUrl,
          pem: clientNode.pem,
          crt: clientNode.crt,
          cacrt: clientNode.cacrt,
          // Node sizing for client nodes
          sizing_info: {
            no_of_cpus: clientSizing.no_of_cpus,
            storage_in_gigs: clientSizing.storage_in_gigs,
            memory_in_gigs: clientSizing.memory_in_gigs
          }
        });
      }
    }
    params.client_nodes = clients;

    this.blockchainService.deploy(params, isOnlyOnPrem).subscribe(response => {
      this.routeService.goToDeploying(response['task_id']);
      this.submitting = false;
      this.wizard.forceFinish();
    }, error => {
      this.submitError = true;
      this.submitErrorMessage = error.message;
      this.submitting = false;
    });
  }

  close() {
    this.isOpen = false;
  }

  doCancel() {
    this.pastContractEngineStep = false;
    this.onCancel();
    this.wizard.close();
  }

  distributeZones() {
    const zones = this.form.controls.nodes['controls'].zones;
    const nodes = this.form.controls.nodes['controls'].numberOfNodes;
    const regionKeys = Object.keys(zones.value);
    const ratio = regionKeys.length;
    const spread = [];

    // Clear out previous distribution
    zones.reset();

    // Create regional spread
    for (let index = 0; index < nodes.value; index++) {
      const idx = index % ratio;
      let val = spread[idx];

      if (val) {
        spread[idx] = ++val;
      } else {
        spread[idx] = 1;
      }
    }

    // Two loops so we don't patch the value multiple times on the same item,
    // because it fires off events each time we do that. This is more efficient.
    for (let index = 0; index < spread.length; index++) {
      const item = zones.controls[regionKeys[index]];
      item.patchValue(spread[index]);
    }
  }

  getActiveZones() {
    // Get onprem OR cloud based on committers zone selection
    switch (this.selectedZoneType) {
      case ZoneType.ON_PREM: return this.onPremZones;
      case ZoneType.VMC_AWS: return this.cloudZones;
    }
    return [];
  }

  getUsedZones() {
    // Get used zones summary for both committers and clients
    const allClientZones = [];
    if (this.selectedEngine === this.engines.DAML) {
      for (let g = 0; g < this.clientGroups.length; ++g) {
        const memberClients = (this.clientGroups.at(g) as FormGroup).controls.member_clients as FormArray;
        for (let c = 0; c < memberClients.length; ++c) {
          const zoneId = (memberClients.at(c) as FormGroup).controls.zone_id as FormControl;
          allClientZones.push(zoneId.value);
        }
      }
    }
    const committerI18N = this.translate.instant('blockchainWizard.committers.singular');
    const committersI18N = this.translate.instant('blockchainWizard.committers.plural');
    const clientI18N = this.translate.instant('blockchainWizard.clients.singular');
    const clientsI18N = this.translate.instant('blockchainWizard.clients.plural');
    const zonesUsageInfo = [];
    this.zones.filter(zone => {
      const value = (this.form.controls.nodes as FormGroup).controls.zones.value[zone.id];
      const committersUsed = (value !== '' && value !== '0') ? parseInt(value, 10) : 0;
      const clientsUsed = allClientZones.filter(zoneId => zoneId === zone.id).length;
      const summaryPhrases = [];
      const commitersPhrase = committersUsed ? `${committersUsed} ${committersUsed > 1 ? committersI18N : committerI18N}` : '';
      const clientsPhrase = clientsUsed ? `${clientsUsed} ${clientsUsed > 1 ? clientsI18N : clientI18N}` : '';
      if (commitersPhrase) { summaryPhrases.push(commitersPhrase); }
      if (clientsPhrase) { summaryPhrases.push(clientsPhrase); }
      if (committersUsed + clientsUsed > 0) {
        zonesUsageInfo.push({
          id: zone.id, name: zone.name,
          committersUsed: committersUsed,
          clientsUsed: clientsUsed,
          summary: summaryPhrases.join(', ')
        });
      }
    });
    return zonesUsageInfo;
  }

  initForm(): FormGroup {
    this.clientGroups = new FormArray([], [Validators.required, this.clientNodesValidator.bind(this)]);
    return new FormGroup({
      details: new FormGroup({
        consortium_name: new FormControl('', Validators.required),
        consortium_desc: new FormControl(''),
      }),
      nodes: new FormGroup({
        numberOfNodes: new FormControl('', Validators.required),
        zones: this.zoneGroup(),
        // onPremZones: this.zoneGroup('onPremZones'),
      }, { validators: RegionCountValidator }),
      clients: this.clientGroups
    });
  }

  addReplicaSize(template: NodeTemplateFormResponse) {
    this.nodeSizingTemplate = template;
  }

  addOnPrem() {
    this.showOnPrem = true;

    setTimeout(() => {
      this.wizard.goTo(this.onPremPage.id);
    }, 100);
  }

  cancelOnPremAdd() {
    this.wizard.goTo(this.replicaPage.id);
    this.showOnPrem = false;
  }

  submitOnPrem(): void {
    this.loadingFlag = true;
    this.onPremForm.addOnPrem().subscribe((zoneData) => {
      const replicas = this.form['controls'].nodes;
      const zones = replicas['controls'].zones;
      const onPremData = zoneData;
      // Disable cloud zones, because we don't have a hybrid setup yet.
      this.zones.forEach(zone => {
        // Disable non on-prem zones
        if (!zone.password) {
          zones['controls'][zone.id].reset();
          zones['controls'][zone.id].disable();
        }
      });

      zones.addControl(onPremData.id, new FormControl('', Validators.required));

      replicas['controls'].numberOfNodes.reset();
      setTimeout(() => {
        this.zones.push(onPremData);
      }, 10);
      this.loadingFlag = false;
      this.wizard.forceNext();
    }, () => {
      this.loadingFlag = false;
    });

  }

  private handlePageChange() {
    const currentPage = this.wizard.currentPage;

    switch (currentPage) {
      case this.detailPage:
        setTimeout(() => {
          this.consortiumInput.nativeElement.focus();
        }, 10);
        break;
      case this.replicaPage:
        this.setupZones();
        break;
      case this.clientsPage:
        if (this.clientGroups.length === 0) {
          setTimeout(() => this.addClientGroup(), 100);
        }
        break;
    }
  }

  private isAuthorized(): boolean {
    const blockchainCount = this.blockchainService.blockchains.length;
    let maxChain = 0;
    if (this.authService.orgProps) {
      maxChain = this.authService.orgProps.max_chains;
    }

    return (maxChain === 0) || (maxChain > blockchainCount);
  }

  private zoneGroup() {
    const group = {};

    this.zones.forEach(zone => {
      group[zone.id] = new FormControl('');
    });

    return new FormGroup(group);
  }

  onClickToHelp(helpId) {
    this.helpService.openHelpPage(helpId);
  }

}
