/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, AfterViewInit, ViewChild, ElementRef } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { map, catchError, debounceTime } from 'rxjs/operators';
import { Observable } from 'rxjs';

import { VmwComboboxItem } from '../../shared/components/combobox/combobox-items/combobox-item.model';
import { OnPremZone, ZoneType } from '../shared/blockchain.model';
import { BlockchainService } from '../shared/blockchain.service';

@Component({
  selector: 'concord-on-premises-form',
  templateUrl: './on-premises-form.component.html',
  styleUrls: ['./on-premises-form.component.scss']
})
export class OnPremisesFormComponent implements AfterViewInit {
  @ViewChild('ipInput') ipInput: ElementRef;
  onPremConnectionSuccessful: boolean;
  addedOnPrem: boolean;
  form: FormGroup;
  onPremZone: OnPremZone;
  onPremError: any;
  locations: any[] = [];
  selectedLocation: any;
  constructor(private blockchainService: BlockchainService) {
    this.form = this.initForm();
  }

  ngAfterViewInit() {
    this.form.controls.onPrem.statusChanges
      .subscribe(status => this.handleOnPremTesting(status));

    this.form.controls.onPremLocation.get('location')
      .valueChanges.subscribe(value => this.handleGetLocation(value));
  }

  getZoneInfo() {
    const onPremLocation = this.form['controls'].onPremLocation;
    const location = onPremLocation['controls'].location.value.value;
    const onPremLocData = onPremLocation.value;

    const id = location.replace(' ', '_');
    this.onPremZone = this.getOnPremInfo();
    this.onPremZone.id = id;
    this.onPremZone.name = `${location} - ${onPremLocData.name}`;

    // TODO - Add Lat long
    // this.onPremZone.latitude = 45.2551139;
    // this.onPremZone.longitude = -120.888727;

    return this.onPremZone;
  }

  addOnPrem(): Observable<OnPremZone> {
    return this.blockchainService.addOnPremZone(this.getZoneInfo()).pipe(
      map(response => {
        this.onPremConnectionSuccessful = false;
        this.addedOnPrem = true;
        this.form.reset();
        this.focusInput();

        return response;
      }),
      // @ts-ignore
      catchError<OnPremZone>(error => {
        this.onPremError = error;

        return error;
      })
    );
  }

  focusInput() {
    setTimeout(() => {
      this.ipInput.nativeElement.focus();
    }, 10);
  }

  private getOnPremInfo() {
    const onPrem = this.form['controls'].onPrem;
    const network = onPrem.value['network'];

    onPrem.value['network'].name_servers = !Array.isArray(network.name_servers) ?
      (network.name_servers.replace(/\s/g, '')).split(',') :
      network.name_servers;
    onPrem.value['network'].ip_pool = !Array.isArray(network.ip_pool) ?
      (network.ip_pool.replace(/\s/g, '')).split(',') :
      network.ip_pool;

    onPrem.value.type = ZoneType.ON_PREM;

    return onPrem.value;
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
          username: new FormControl('', { validators: Validators.required, updateOn: 'blur' }),
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
        }),
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
        })
      }),
      onPremLocation: new FormGroup({
        name: new FormControl('', Validators.required),
        location: new FormControl('', Validators.required),
      }),
    });
  }

  private handleOnPremTesting(status: string) {
    if (status === 'VALID') {
      this.onPremError = undefined;
      const testCon = this.blockchainService
        .testOnPremZoneConnection(this.getOnPremInfo())
        .subscribe(() => {
          this.onPremConnectionSuccessful = true;
          testCon.unsubscribe();
        }, error => {
          this.onPremError = error;
          testCon.unsubscribe();
          return error;
        });
    }

  }

  private handleGetLocation(item: VmwComboboxItem) {
    if (item && item.value.length > 5) {
      this.blockchainService.getZoneLatLong(item.value).pipe(debounceTime(1200))
        .subscribe(locations => this.locations = locations);
    }
  }

}
