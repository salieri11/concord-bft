/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, AfterViewInit, ViewChild } from '@angular/core';
import { Router } from '@angular/router';
import { ClrWizard } from '@clr/angular';

import { OnPremisesFormComponent } from '../on-premises-form/on-premises-form.component';

@Component({
  selector: 'concord-on-premises-modal',
  templateUrl: './on-premises-modal.component.html',
  styleUrls: ['./on-premises-modal.component.scss']
})
export class OnPremisesModalComponent implements AfterViewInit {
  @ViewChild('form') form: OnPremisesFormComponent;
  @ViewChild('modal') modal: ClrWizard;
  isOpen: boolean;
  adding: boolean;

  constructor(private router: Router) { }

  ngAfterViewInit() {
  }

  addOnPrem() {
    this.adding = true;
    this.form.addOnPrem().subscribe(() => {
        this.adding = false;
    });
  }

  open(): void {
    this.isOpen = true;
    this.form.focusInput();
  }

  resetFragment() {
    this.router.navigate([]);
  }

}
