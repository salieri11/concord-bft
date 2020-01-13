/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, AfterViewInit, ViewChild, ElementRef } from '@angular/core';
import { FormControl, FormGroup, FormArray, Validators } from '@angular/forms';
import { map, catchError, debounceTime, distinctUntilChanged } from 'rxjs/operators';
import { Observable, throwError } from 'rxjs';

import { VmwComboboxItem } from '../../shared/components/combobox/combobox-items/combobox-item.model';
import { OnPremZone, ZoneType } from './../../zones/shared/zones.model';
import { ZonesService } from '../shared/zones.service';
import { BlockchainService } from './../../blockchain/shared/blockchain.service';


@Component({
  selector: 'concord-zone-form',
  templateUrl: './zone-form.component.html',
  styleUrls: ['./zone-form.component.scss']
})
export class ZoneFormComponent implements AfterViewInit {
  @ViewChild('ipInput', { static: true }) ipInput: ElementRef;
  onPremConnectionSuccessful: boolean;
  addedOnPrem: boolean;
  form: FormGroup;
  onPremZone: OnPremZone;
  onPremError: any;
  locations: any[] = [];
  selectedLocation: any;

  get log_managements() { return this.form.get('log_managements'); }

  constructor(
    private zonesService: ZonesService,
    private blockchainService: BlockchainService
  ) {
    this.form = this.initForm();
  }

  ngAfterViewInit() {
    this.form.controls.onPrem.statusChanges
      .subscribe(status => this.handleOnPremTesting(status));

    this.form.controls.onPremLocation.get('location')
      .valueChanges.pipe(
        debounceTime(500),
        distinctUntilChanged()
      ).subscribe(value => this.handleGetLocation(value));
  }

  getZoneInfo() {
    const onPremLocData = this.form['controls'].onPremLocation.value;

    // When updating we need to grab the first index
    if (Array.isArray(onPremLocData.location)) {
      onPremLocData.location = onPremLocData.location[0];
    }

    const id = onPremLocData.location.value.replace(' ', '_');

    this.onPremZone = this.getOnPremInfo();
    this.onPremZone.id = id;
    this.onPremZone.name = `${onPremLocData.location.value} - ${onPremLocData.name}`;
    this.onPremZone.latitude = onPremLocData.location && onPremLocData.location.geometry ? onPremLocData.location.geometry.lat : null;
    this.onPremZone.longitude = onPremLocData.location && onPremLocData.location.geometry ? onPremLocData.location.geometry.long : null;

    return this.onPremZone;
  }

  addOnPrem(): Observable<OnPremZone> {

    return this.zonesService.addZone(this.getZoneInfo()).pipe(
      map(response => this.handleSave(response)),
      // @ts-ignore
      catchError<OnPremZone>(error => this.handleError(error))
    );
  }

  update(id: string): Observable<OnPremZone> {
    return this.zonesService.update(id, this.getZoneInfo()).pipe(
      map(response => this.handleSave(response)),
      // @ts-ignore
      catchError<OnPremZone>(error => this.handleError(error))
    );
  }

  private getOnPremInfo() {
    const formValues = this.form.value;
    const onPrem = this.form['controls'].onPrem;
    const network = onPrem.value['network'];

    onPrem.value['network'].name_servers = network.name_servers && network.name_servers.length === 0 ? [] : network.name_servers;
    onPrem.value['network'].name_servers = !Array.isArray(network.name_servers) && network.name_servers ?
      (network.name_servers.replace(/\s/g, '')).split(',') :
      network.name_servers;
    onPrem.value['network'].ip_pool = network.ip_pool && !Array.isArray(network.ip_pool) ?
      (network.ip_pool.replace(/\s/g, '')).split(',') :
      network.ip_pool;

    onPrem.value.type = ZoneType.ON_PREM;

    // Remove onPrem info that hasn't been massaged
    delete formValues.onPrem;

    return {...formValues, ...onPrem.value};
  }

  private initForm(): FormGroup {
    // tslint:disable-next-line
    const urlRegEx = /^(https?:\/\/)?((([a-z\d]([a-z\d-]*[a-z\d])*)\.)+[a-z]{2,}|((\d{1,3}\.){3}\d{1,3}))(\:\d+)?(\/[-a-z\d%_.~+]*)*(\?[;&a-z\d%_.~+=-]*)?(\#[-a-z\d_]*)?$/i;
    const ipRegEx = /^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$/gm;
    const listOfIpsRegEx = new RegExp(['^((((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]',
      '|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.',
      '(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\,?)*)-?)*$'].join(''));

    return new FormGroup({
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
              validators: [Validators.pattern(urlRegEx)],
              updateOn: 'blur'
            }
          ),
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
      outbound_proxy: new FormGroup({
        http_host: new FormControl(
          '',
          {
            validators: Validators.pattern(urlRegEx),
            updateOn: 'blur'
          }
        ),
        http_port: new FormControl('', {
          validators: Validators.maxLength(4), updateOn: 'blur'
        }),
        https_host: new FormControl(
          '',
          {
            validators: Validators.pattern(urlRegEx),
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
  }

  private handleSave(response: OnPremZone) {
    this.onPremConnectionSuccessful = false;
    this.addedOnPrem = true;
    this.form.reset();
    this.blockchainService.getZones().subscribe();

    return response;
  }

  private handleError(error) {
    this.onPremError = error;

    return throwError(error);
  }

  private handleOnPremTesting(status: string) {
    if (status === 'VALID') {
      this.onPremError = undefined;
      const testCon = this.zonesService
        .testOnPremZoneConnection(this.getOnPremInfo())
        .subscribe(() => {
          this.onPremConnectionSuccessful = true;
          testCon.unsubscribe();
        }, error => {
          this.onPremConnectionSuccessful = false;
          this.onPremError = error.message;
          testCon.unsubscribe();
          return error;
        });
    }

  }

  private handleGetLocation(item: VmwComboboxItem) {
    if (item && item.value && item.value.length > 3) {
      this.zonesService.getZoneLatLong(item.value)
        .subscribe(locations => this.locations = locations);
    }
  }

}
