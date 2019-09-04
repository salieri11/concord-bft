/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, AfterViewInit, ViewChild, ElementRef } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { map, catchError, debounceTime } from 'rxjs/operators';
import { Observable } from 'rxjs';

import { VmwComboboxItem } from '../../shared/components/combobox/combobox-items/combobox-item.model';
import { Zone } from '../shared/blockchain.model';
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
  onPremZone: Zone;
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
    const id = location.replace(' ', '_');
    const onPremLocData = onPremLocation.value;
    console.log(location);

    this.onPremZone = this.getOnPremInfo();
    this.onPremZone.id = id;
    this.onPremZone.name = `${location} - ${onPremLocData.name}`;

    return this.onPremZone;
  }

  addOnPrem(): Observable<Zone> {
    return this.blockchainService.addOnPremZone(this.getZoneInfo()).pipe(
      map(response => {
        this.onPremConnectionSuccessful = false;
        this.addedOnPrem = true;
        this.form.reset();
        this.focusInput();

        return response;
      }),
      // @ts-ignore
      catchError<Zone>(error => {
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

  onLocationSelected(location) {
    console.log(location);
  }

  private getOnPremInfo() {
    const onPrem = this.form['controls'].onPrem;
    return onPrem.value;
  }

  private initForm(): FormGroup {
    // tslint:disable-next-line
    const urlRegEx = /^(https?:\/\/)?((([a-z\d]([a-z\d-]*[a-z\d])*)\.)+[a-z]{2,}|((\d{1,3}\.){3}\d{1,3}))(\:\d+)?(\/[-a-z\d%_.~+]*)*(\?[;&a-z\d%_.~+=-]*)?(\#[-a-z\d_]*)?$/i;

    return new FormGroup({
      onPrem: new FormGroup({
        ip: new FormControl(
          '',
          {
            validators: [Validators.required, Validators.pattern(urlRegEx)],
            updateOn: 'blur'
          }
        ),
        username: new FormControl('', { validators: Validators.required, updateOn: 'blur' }),
        password: new FormControl('', { validators: Validators.required, updateOn: 'blur' }),
        compNetwork: new FormControl('', { validators: Validators.required, updateOn: 'blur' }),
        resourcePool: new FormControl('', { validators: Validators.required, updateOn: 'blur' }),
        folder: new FormControl('', { validators: Validators.required, updateOn: 'blur' }),
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
