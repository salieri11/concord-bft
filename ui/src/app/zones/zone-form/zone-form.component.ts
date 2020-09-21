/*
 * Copyright 2018-2020 VMware, all rights reserved.
 */

import { Component, AfterViewInit, Input, ViewChild } from '@angular/core';
import { FormControl, FormGroup, FormArray, Validators, AbstractControl } from '@angular/forms';
import { map, catchError, debounceTime, distinctUntilChanged, tap } from 'rxjs/operators';
import { ActivatedRoute } from '@angular/router';
import { Observable, throwError } from 'rxjs';

import { VmwComboboxItem } from '../../shared/components/combobox/combobox-items/combobox-item.model';
import { protocolNotAllowed, urlRegEx, ipRegEx, listOfIpsRegEx } from '../../shared/custom-validators';
import { OnPremZone, ZoneType } from './../../zones/shared/zones.model';
import { ZonesService } from '../shared/zones.service';
import { BlockchainService } from './../../blockchain/shared/blockchain.service';
import { VmwComboboxComponent } from '../../shared/components/combobox/combobox.component';


@Component({
  selector: 'concord-zone-form',
  templateUrl: './zone-form.component.html',
  styleUrls: ['./zone-form.component.scss']
})
export class ZoneFormComponent implements AfterViewInit {
  @Input('formVisible') formVisible: boolean = false;
  @ViewChild('locationInput', { static: false }) locationInput: VmwComboboxComponent;

  form: FormGroup;
  onPremConnectionSuccessful: boolean;
  onPremConnectionInProgress: boolean = false;
  onPremConnectionLastTested: string = '';
  otherValidationsFailed: boolean = false;
  canDelete: boolean;
  settingZone: boolean = false;
  inputUpdateLocker = {};
  enableRealtimeValidation = true;
  metricsSelection = new FormControl('');
  metricSelected = 'wavefront';

  insufficientSection = {
    logging: true,
    wavefront: true,
    elasticsearch: true,
    metrics: true,
    containerReg: true,
    outboundProxy: true,
  };
  someValueMap = {
    logging: false,
    wavefront: false,
    elasticsearch: false,
    metrics: false,
    containerReg: false,
    outboundProxy: false,
  };

  onPremZone: OnPremZone;
  onPremFailed: boolean;
  onPremError: boolean;
  locations: any[] = [];
  locationCache = {};
  selectedLocation: any;
  zoneId: string;

  originalValue: string = '';
  changedFromOriginalValue: boolean = false;

  get log_managements() { return this.form.get('log_managements'); }

  constructor(
    private zonesService: ZonesService,
    private blockchainService: BlockchainService,
    private route: ActivatedRoute,
  ) {
    this.initForm();

    this.route.params.subscribe(params => {
      this.zoneId = params['zoneId'];
      this.zonesService.canDelete(this.zoneId).subscribe(canDelete =>   this.canDelete = canDelete);
    });
  }

  ngAfterViewInit() {
    // Dummy call
    if (this.handleOnPremTesting) { this.onPremConnectionInProgress = false; }

    this.form.controls.onPremLocation.get('location')
      .valueChanges.pipe(
        tap(location => this.handleSuggestionToggle(location)),
        debounceTime(500),
        distinctUntilChanged()
      ).subscribe(value => this.handleGetLocation(value));

    this.form.statusChanges.subscribe(status => {
      this.otherValidationsFailed = this.checkValidationsFailed(status);
      this.updateSectionEmpty();
    });

    this.form.valueChanges.subscribe(_ => {
      if (this.settingZone) { return; } // Skip while setting entire zone values
      const newValue = JSON.stringify(this.getEffectiveZoneData());
      if (newValue) { this.changedFromOriginalValue = (newValue !== this.originalValue); }
    });
  }

  addOnPrem(): Observable<OnPremZone> {
    return this.zonesService.addZone(this.getEffectiveZoneData()).pipe(
      map(response => this.handleSave(response)),
      // @ts-ignore
      catchError<OnPremZone>(error => this.handleError(error))
    );
  }

  update(id: string): Observable<OnPremZone> {
    return this.zonesService.update(id, this.getEffectiveZoneData()).pipe(
      map(response => this.handleSave(response)),
      // @ts-ignore
      catchError<OnPremZone>(error => this.handleError(error))
    );
  }

  // Real-time form validation (not dependent on blur event)
  onchange(event, formControlPath?: string) {
    if (!this.enableRealtimeValidation || !formControlPath) { return; }
    const el = event.target;
    if (!el._refreshed) { el.blur(); el.focus(); el._refreshed = true; }
    const delay = el.value ? 500 : 0;
    const locker = this.inputUpdateLocker = {};
    setTimeout(() => { // after some delay update form value and validate
      if (locker === this.inputUpdateLocker) {
        const ctrl = this.getFormControlFromPath(formControlPath);
        if (ctrl) {
          ctrl.setValue(el.value);
          this.form.updateValueAndValidity();
        }
      }
    }, delay);
  }

  onMetricsSelect(event) {
    const v = event.target.value;
    this.metricSelected = v;
    // Clear other form; submission must be elasticsearch OR wavefront
    if (v === 'elasticsearch') {
      this.form.controls.metrics.get('wavefront').get('url').setValue('');
      this.form.controls.metrics.get('wavefront').get('token').setValue('');
    } else if (v === 'wavefront')  {
      this.form.controls.metrics.get('elasticsearch').get('url').setValue('');
      this.form.controls.metrics.get('elasticsearch').get('username').setValue('');
      this.form.controls.metrics.get('elasticsearch').get('password').setValue('');
    }
  }

  getVCenterCredentialIndex() {
    const onPrem = this.form.controls.onPrem.value;
    const vcenter = this.form.controls.onPrem.get('vcenter').value;
    const network = this.form.controls.onPrem.get('network').value;
    const anyEmpty = (!vcenter.url || !vcenter.username || !vcenter.password || !onPrem.folder
                        || !onPrem.resource_pool || !onPrem.storage || !network.name);
    if (anyEmpty && this.onPremConnectionSuccessful) {
      this.onPremConnectionSuccessful = false;
    }
    return {
      empty: anyEmpty,
      index: [vcenter.url, vcenter.username, vcenter.password,
              onPrem.resource_pool, onPrem.storage, onPrem.folder,
              network.name].join('|')
    };
  }

  getEffectiveZoneData() {
    let formValues = this.form.value;
    const onPrem = this.form.controls.onPrem;
    const network = onPrem.value.network;
    const metrics = this.form.controls.metrics;
    const onPremLocData = this.form.controls.onPremLocation.value;

    // When updating we need to grab the first index
    if (Array.isArray(onPremLocData.location)) {
      onPremLocData.location = onPremLocData.location[0];
    }

    onPrem.value.name = `${onPremLocData.location.value} - ${onPremLocData.name}`;
    const hasCoordinate = (onPremLocData.location && onPremLocData.location.geometry && onPremLocData.location.geometry.lat);
    onPrem.value.latitude = hasCoordinate ? onPremLocData.location.geometry.lat : null;
    onPrem.value.longitude = hasCoordinate ? onPremLocData.location.geometry.long : null;

    // Mock if these mandatory network fields are not given, this means ui is testing vCenter creds
    // For testing only network fields can be ignored, becuase this section don't get checked on Helen
    const onPremNet = onPrem.value.network;
    if (!onPremNet.gateway || !onPremNet.subnet || !onPremNet.name_servers || !onPremNet.ip_pool) {
      onPrem.value.network.gateway = '0.0.0.0';
      onPrem.value.network.subnet = '24';
      onPrem.value.network.name_servers = ['0.0.0.0', '0.0.0.0'];
      onPrem.value.network.ip_pool = ['0.0.0.0', '0.0.0.0'];
    } else {
      onPrem.value.network.name_servers = network.name_servers && network.name_servers.length === 0 ? [] : network.name_servers;
      onPrem.value.network.name_servers = !Array.isArray(network.name_servers) && network.name_servers ?
        (network.name_servers.replace(/\s/g, '')).split(',') :
        network.name_servers;
      onPrem.value.network.ip_pool = network.ip_pool && !Array.isArray(network.ip_pool) ?
        (network.ip_pool.replace(/\s/g, '')).split(',') :
        network.ip_pool;
    }
    onPrem.value.type = ZoneType.ON_PREM;

    if (formValues.onPrem) { delete formValues.onPrem; } // Remove onPrem info that hasn't been massaged
    if (formValues.metrics) { delete formValues.metrics; } // Metrics get added in with wavefront and elasticsearch.
    formValues = this.validateData(formValues);

    const submittableData = {
      ...formValues,
      ...onPrem.value,
      wavefront: metrics.get('wavefront').value,
      elasticsearch: metrics.get('elasticsearch').value,
    };

    return submittableData;
  }

  private validateData(formValues: any): any {
    // Temporary till we refactor the form with the stepper
    // If no values added set to null or empty array
    if (this.insufficientSection.logging) {
      formValues.log_managements = [];
    }
    if (this.insufficientSection.containerReg) {
      formValues.container_repo = { url: '', username: '', password: '' };
    }
    if (this.insufficientSection.wavefront) {
      formValues.wavefront = { url: '', token: '' };
    }
    if (this.insufficientSection.elasticsearch) {
      formValues.elasticsearch = { url: '', username: '', password: '' };
    }
    if (this.insufficientSection.outboundProxy) {
      formValues.outbound_proxy = { http_host: '', http_port: '', https_host: '', https_port: '', };
    }
    return formValues;
  }

  private handleSave(response: OnPremZone): OnPremZone {
    this.onPremConnectionSuccessful = false;
    // Update zones in blockchainservice
    this.blockchainService.getZones().subscribe();
    this.form.markAsUntouched();
    return response;
  }

  private handleError(error) {
    this.onPremFailed = error;
    return throwError(error);
  }

  private checkValidationsFailed(status?: string) {
    const ctrls = this.form.controls;
    if (status) {
      if (status === 'INVALID') { return true; }
    } else if (ctrls.onPrem.invalid || ctrls.onPremLocation.invalid) {
      return true;
    }
    return false;
  }

  private getFormControlFromPath(path: string) {
    try {
      const steps = path.split('.');
      let ctrl: FormGroup | AbstractControl = this.form;
      for (const step of steps) { ctrl = ctrl.get(step); }
      return ctrl;
    } catch (e) { console.error(e); return null; }
  }

  private updateSectionEmpty() {
    const ctrls = this.form.controls;
    // Has all field values
    const logFieldSkip = ['log_insight_agent_id', 'destination'];
    this.insufficientSection.logging = !allMembersHaveValue(ctrls.log_managements.value, logFieldSkip);
    this.insufficientSection.wavefront = !allKeysHaveValue(ctrls.metrics.get('wavefront').value);
    this.insufficientSection.elasticsearch = !allKeysHaveValue(ctrls.metrics.get('elasticsearch').value);
    this.insufficientSection.metrics = this.insufficientSection.wavefront && this.insufficientSection.elasticsearch;
    this.insufficientSection.containerReg = !allKeysHaveValue(ctrls.container_repo.value);
    this.insufficientSection.outboundProxy = !allKeysHaveValue(ctrls.outbound_proxy.value);
    // Some fields have value
    this.someValueMap.logging = someMembersHaveValue(ctrls.log_managements.value, logFieldSkip);
    this.someValueMap.wavefront = someKeysHaveValue(ctrls.metrics.get('wavefront').value);
    this.someValueMap.elasticsearch = someKeysHaveValue(ctrls.metrics.get('elasticsearch').value);
    this.someValueMap.metrics = this.someValueMap.wavefront || this.someValueMap.elasticsearch;
    this.someValueMap.containerReg = someKeysHaveValue(ctrls.container_repo.value);
    this.someValueMap.outboundProxy = someKeysHaveValue(ctrls.outbound_proxy.value);
    const vCenterChanged = this.onPremConnectionLastTested !== this.getVCenterCredentialIndex().index;
    if (vCenterChanged) { this.onPremConnectionSuccessful = false; }
  }

  // Only for stepper re-populating fields for update
  async afterLoadingFormForUpdate() {
    const wait = async (ms) => new Promise(r => { setTimeout(() => { r(); }, ms); });
    const buttonIds = [ 'nextButtonNameLocation', 'nextButtonVCenter', 'nextButtonLogManagement',
                      'nextButtonMetricsManagement', 'nextButtonContainerRegistry', 'nextButtonOutboundProxy'];
    for (const buttonId of buttonIds) {
      for (let i = 0; i < 50; ++i) {
        const a = document.getElementById(buttonId);
        if (a) { a.click(); await wait(50); break; }
        await wait(10);
      }
    }
    return true;
  }

  private handleOnPremTesting() {
    const vCenterInputs = ['vcUrl', 'vcUsername', 'vcPassword', 'vcRp', 'vcStorage', 'vcFolder', 'netName'];
    setTimeout(() => {
      // Still filling out vCenter, no need to check yet.
      if (vCenterInputs.indexOf(document.activeElement.id) >= 0) { return; }
      if (this.onPremConnectionInProgress) { return; }
      const vcenterInfo = this.getVCenterCredentialIndex();
      if (vcenterInfo.empty && !this.onPremConnectionSuccessful) { return; }
      if (vcenterInfo.index === this.onPremConnectionLastTested) { return; }
      this.onPremConnectionInProgress = true;
      this.onPremConnectionSuccessful = false;
      this.onPremConnectionLastTested = vcenterInfo.index;
      this.onPremFailed = undefined;
      const testCon = this.zonesService
        .testOnPremZoneConnection(this.getEffectiveZoneData())
        .subscribe(response => {
          this.onPremError = false;
          if (response['result'] && response['result'] === 'UNKNOWN') {
            this.onPremConnectionSuccessful = false;
            this.onPremConnectionInProgress = false;
            this.onPremFailed = true;
            testCon.unsubscribe();
            return;
          }
          this.onPremConnectionSuccessful = true;
          this.onPremConnectionInProgress = false;
          testCon.unsubscribe();
        }, error => {
          this.onPremConnectionSuccessful = false;
          this.onPremConnectionInProgress = false;
          this.onPremFailed = false;
          this.onPremError = true;
          testCon.unsubscribe();
          return error;
        });
    }, 0);
  }

  private handleSuggestionToggle(location) {
    if (this.settingZone) { return; } // Skip while setting entire zone values
    location = Array.isArray(location) ? location[0] : location;
    const valueShort = !location || (location.value !== undefined
                        && location.value.length < 4);
    const cachedLoc = this.locationCache[location.value];
    const locationIsCached = cachedLoc ? true : false;
    if (valueShort || locationIsCached) {
      this.locationInput.showLoading = false;
      this.locationInput.showSuggestions = false;
      return;
    }
    if (this.locationInput) {
      this.locationInput.showLoading = true;
      this.locationInput.showSuggestions = true;
    }
  }

  private handleGetLocation(item: VmwComboboxItem) {
    if (item && item.value && item.value.length > 3) {
      this.zonesService.getZoneLatLong(item.value)
        .subscribe(locations => {
          for (const location of locations) {
            this.locationCache[location.value] = location;
          }
          if (this.locationInput) { this.locationInput.showLoading = false; }
          this.locations = locations;
        });
    }
  }

  private initForm(): FormGroup {
    // tslint:disable-next-line
    this.form = new FormGroup({
      onPrem: new FormGroup({
        vcenter: new FormGroup({
          url: new FormControl(
            '',
            {
              validators: [Validators.required, Validators.pattern(urlRegEx)],
              updateOn: 'blur'
            }
          ),
          username: new FormControl('', { updateOn: 'blur' }),
          password: new FormControl('', { validators: Validators.required, updateOn: 'blur' })
        }),
        resource_pool: new FormControl('', { validators: Validators.required, updateOn: 'blur' }),
        storage: new FormControl('', { validators: Validators.required, updateOn: 'blur' }),
        folder: new FormControl('', { validators: Validators.required, updateOn: 'blur' }),
        network: new FormGroup({
          name: new FormControl('', { validators: Validators.required, updateOn: 'blur' }),
          gateway: new FormControl('', { validators: [Validators.required, Validators.pattern(ipRegEx)], updateOn: 'blur' }),
          subnet: new FormControl('', { validators: Validators.required, updateOn: 'blur' }),
          name_servers: new FormControl('', { validators: Validators.pattern(listOfIpsRegEx), updateOn: 'blur' }),
          ip_pool: new FormControl('', { validators: [Validators.required, Validators.pattern(listOfIpsRegEx)], updateOn: 'blur' })
        })
      }),
      log_managements: new FormArray([
        new FormGroup({
          destination: new FormControl('LOG_INSIGHT'),
          address: new FormControl(
            '',
            {
              validators: [protocolNotAllowed({optional: true})],
              updateOn: 'blur'
            }
          ),
          port: new FormControl(null, {
            validators: Validators.maxLength(4), updateOn: 'blur'
          }),
          username: new FormControl('', { updateOn: 'blur' }),
          password: new FormControl('', { updateOn: 'blur' }),
          log_insight_agent_id: new FormControl(100)
        })
      ]),
      container_repo: new FormGroup({
        url: new FormControl(
          '',
          {
            validators: Validators.pattern(urlRegEx),
            updateOn: 'blur'
          }
        ),
        username: new FormControl('', { updateOn: 'blur' }),
        password: new FormControl('', { updateOn: 'blur' })
      }),
      metrics: new FormGroup({
        wavefront: new FormGroup({
          url: new FormControl(
            '',
            {
              validators: Validators.pattern(urlRegEx),
              updateOn: 'blur'
            }
          ),
          token: new FormControl('', { updateOn: 'blur' }),
        }),
        elasticsearch: new FormGroup({
          url: new FormControl(
            '',
            {
              validators: Validators.pattern(urlRegEx),
              updateOn: 'blur'
            }
          ),
          username: new FormControl('', { updateOn: 'blur' }),
          password: new FormControl('', { updateOn: 'blur' })
        }),
      }),
      outbound_proxy: new FormGroup({
        http_host: new FormControl(
          '',
          {
            validators: [protocolNotAllowed({optional: true})],
            updateOn: 'blur'
          }
        ),
        http_port: new FormControl(null, {
          validators: Validators.maxLength(4), updateOn: 'blur'
        }),
        https_host: new FormControl(
          '',
          {
            validators: [protocolNotAllowed({optional: true})],
            updateOn: 'blur'
          }
        ),
        https_port: new FormControl('', {
          validators: Validators.maxLength(4), updateOn: 'blur'
        }),
      }),
      onPremLocation: new FormGroup({
        name: new FormControl('', Validators.required),
        location: new FormControl('', Validators.required),
      }),
    });
    return this.form;
  }

}

function allMembersHaveValue(array: any[], skip: string[] = []): boolean {
  let result = true;
  for (const member of array) {
    if (!member) { result = false; break; }
    if (typeof member === 'object' && !allKeysHaveValue(member, skip)) { result = false; break; }
    if (Array.isArray(member) && !allMembersHaveValue(member, skip)) { result = false; break; }
  }
  return result;
}

function allKeysHaveValue(obj: object, skip: string[] = []) {
  let result = true;
  for (const key of Object.keys(obj)) {
    if (skip.indexOf(key) >= 0) { continue; }
    if (!obj[key]) { result = false; break; }
  }
  return result;
}

function someMembersHaveValue(array: any[], skip: string[] = []): boolean {
  let result = false;
  for (const member of array) {
    if (!member) { continue; }
    if (typeof member === 'object' && someKeysHaveValue(member, skip)) { result = true; break; }
    if (Array.isArray(member) && someMembersHaveValue(member, skip)) { result = true; break; }
  }
  return result;
}

function someKeysHaveValue(obj: object, skip: string[] = []) {
  let result = false;
  for (const key of Object.keys(obj)) {
    if (skip.indexOf(key) >= 0) { continue; }
    if (obj[key]) { result = true; break; }
  }
  return result;
}
