/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, AfterViewInit, ViewChild, ElementRef } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { map } from 'rxjs/operators';
import { Observable } from 'rxjs';

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

  constructor(private blockchainService: BlockchainService) {
    this.form = this.initForm();
  }

  ngAfterViewInit() {
    localStorage.removeItem('zone');

    this.form.controls.onPrem.statusChanges.subscribe(status => {
      if (status === 'VALID') {
        setTimeout(() => {
          this.onPremConnectionSuccessful = true;

        }, 1500);
      }
    });
  }

  getZoneInfo() {
    const onPrem = this.form['controls'].onPrem;
    const onPremLocation = this.form['controls'].onPremLocation;
    const location = onPremLocation['controls'].location.value;
    const id = location.replace(' ', '_');
    const onPremLocData = onPremLocation.value;

    this.onPremZone = onPrem.value;

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

          localStorage.setItem('zone', JSON.stringify(response));
        return response;
      })
    );
  }

  focusInput() {
    setTimeout(() => {
      this.ipInput.nativeElement.focus();
    }, 10);
  }

  private initForm(): FormGroup {
    return new FormGroup({
      onPrem: new FormGroup({
        ip: new FormControl('', Validators.required),
        username: new FormControl('', Validators.required),
        password: new FormControl('', Validators.required),
        compNetwork: new FormControl('', Validators.required),
        resourcePool: new FormControl('', Validators.required),
        folder: new FormControl('', [Validators.required, Validators.minLength(5)]),
      }),
      onPremLocation: new FormGroup({
        name: new FormControl('', Validators.required),
        location: new FormControl('', Validators.required),
      }),
    });

  }

}
