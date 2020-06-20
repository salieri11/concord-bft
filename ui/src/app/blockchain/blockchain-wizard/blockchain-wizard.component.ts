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
import { BlockchainRequestParams, ContractEngines, NodeClientParam } from '../shared/blockchain.model';
import { ConsortiumStates, mainRoutes } from '../../shared/urls.model';
import { Zone, ZoneType } from '../../zones/shared/zones.model';
import { ZoneFormComponent } from '../../zones/zone-form/zone-form.component';
import { RouteService } from '../../shared/route.service';
import { ContextualHelpService } from './../../shared/contextual-help.service';

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
  @ViewChild('onPremForm', { static: false }) onPremForm: ZoneFormComponent;
  @ViewChild('consortiumInput', { static: false }) consortiumInput: ElementRef;
  @ViewChild('clientAuthUrlInput', { static: false }) clientAuthUrlInput: ElementRef;

  @Output('events') events: EventEmitter<EventData> = new EventEmitter<EventData>();

  selectedEngine: string;
  isOpen = false;
  form: FormGroup;

  personaOptions = PersonaService.getOptions();
  numbersOfNodes = [4, 7];
  fCountMapping = { '4': 1, '7': 2 };
  zones: Zone[] = [];
  onPremZones: Zone[] = [];
  cloudZones: Zone[] = [];
  engines = ContractEngines;

  showOnPrem: boolean;
  loadingFlag: boolean;

  onPremActive: boolean;
  cloudActive: boolean;
  hasOnPrem: boolean = true;

  pastContractEngineStep: boolean = false;
  clientForm: FormGroup;

  constructor(
    public blockchainService: BlockchainService,
    private authService: AuthenticationService,
    private routeService: RouteService,
    private router: Router,
    private helpService: ContextualHelpService
  ) {
    if (!this.isAuthorized()) {
      this.router.navigate([mainRoutes.forbidden]);
    }

    // this.filterZones();
    this.form = this.initForm();
    this.onPremActive = this.hasOnPrem = this.blockchainService.zones.some(zone => zone.type === ZoneType.ON_PREM);
    this.clientForm = new FormGroup({
      zone_id: new FormControl('', Validators.required),
      auth_url: new FormControl('', [Validators.pattern(urlValidateRegex)]),
    });

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

    // this.filterZones();

    if (this.form && this.wizard) {
      this.wizard.reset();
      this.form.reset();
    }
  }

  tabSelect() {
    this.setupZones();
  }

  // Client node adding related
  addClientNode() {
    const clientsFormArray = (this.form.controls.clients as FormArray);
    const zoneId = this.clientForm.get('zone_id').value;
    const zoneName =  this.blockchainService.zones
                            .filter(zone => zone.id === zoneId)[0].name;
    const clientData = new FormControl({
      zone_id: new FormControl(zoneId),
      auth_url_jwt: new FormControl(this.clientForm.get('auth_url').value),
      zone_name: zoneName
    });
    clientsFormArray.push(clientData);
  }
  removeClientNode(i: number) {
    const clientsFormArray = (this.form.controls.clients as FormArray);
    clientsFormArray.removeAt(i);
  }
  clearClientNodes() {
    const clientsFormArray = (this.form.controls.clients as FormArray);
    clientsFormArray.clear();
  }
  clientNodeValidator(fa: FormArray): ValidationErrors | null {
    if (fa.value.length === 0) { return { 'countIsCorrect': false }; }
    return { 'countIsCorrect': true };
  }

  private setupZones() {
    this.hasOnPrem = this.blockchainService.zones.some(zone => zone.type === ZoneType.ON_PREM);
    const onPremZones = this.blockchainService.zones.filter((zone) => zone.type === ZoneType.ON_PREM);
    const cloudZones = this.blockchainService.zones.filter((zone) => zone.type === ZoneType.VMC_AWS);
    if (this.form) {
      const zones = this.form['controls'].nodes['controls'].zones;
      const pastZones = this.zones;
      this.zones = [];
      pastZones.forEach(zone => {
        zones.removeControl(zone.id);
      });

      if (this.onPremActive) {
        onPremZones.forEach(zone => {
          zones.addControl(zone.id, new FormControl('', Validators.required));
        });
        this.zones = onPremZones;
      } else {
        this.cloudActive = true;
        cloudZones.forEach(zone => {
          zones.addControl(zone.id, new FormControl('', Validators.required));
        });
        this.zones = cloudZones;
      }

    }

    this.distributeZones();
  }

  onSubmit() {
    const params = new BlockchainRequestParams();
    params.consortium_name = this.form.value.details.consortium_name;
    params.blockchain_type = this.selectedEngine;

    // Handle zones info
    const zones = this.form.controls.nodes['controls'].zones.value;
    const isOnlyOnPrem = this.zones.some(zone => zone.type === ZoneType.ON_PREM);
    let zoneIds = [];
    Object.keys(zones).forEach(zoneId => zoneIds = zoneIds.concat(Array(Number(zones[zoneId])).fill(zoneId)));
    params.replica_zone_ids = zoneIds;

    // Handle client nodes
    const clients: NodeClientParam[] = [];
    for (const client of (this.form.get('clients') as FormArray).getRawValue()) {
      clients.push({
        zone_id: client.zone_id.value,
        auth_url_jwt: client.auth_url_jwt.value,
      });
    }
    params.client_nodes = clients;

    /**
     * Sometimes API request takes seconds to return, then, UI will hang at a blank
     * template (with wizard closed/submitted) which is likely to throw off the user.
     * So route user right away to deploying page before response, and once the
     * response returns :taskId will overridden by response task_id.
     * */
    this.routeService.goToDeploying(ConsortiumStates.waiting);

    this.blockchainService.deploy(params, isOnlyOnPrem).subscribe(response => {
      this.routeService.goToDeploying(response['task_id']);
    }, error => {
      this.events.emit({ type: 'error', data: error });
      console.log(error);
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

  initForm(): FormGroup {
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
      clients: new FormArray([], this.clientNodeValidator),
    });
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
    }
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
