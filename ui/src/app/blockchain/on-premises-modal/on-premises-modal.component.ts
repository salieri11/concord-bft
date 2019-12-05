/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, AfterViewInit, ViewChild } from '@angular/core';
import { Router } from '@angular/router';
import { ClrWizard } from '@clr/angular';

import { OnPremisesFormComponent } from '../on-premises-form/on-premises-form.component';
import { ContextualHelpService } from './../../shared/contextual-help.service';

@Component({
  selector: 'concord-on-premises-modal',
  templateUrl: './on-premises-modal.component.html',
  styleUrls: ['./on-premises-modal.component.scss']
})
export class OnPremisesModalComponent implements AfterViewInit {
  @ViewChild('form', { static: true }) form: OnPremisesFormComponent;
  @ViewChild('modal', { static: true }) modal: ClrWizard;
  isOpen: boolean;
  adding: boolean;

  constructor(private router: Router, private helpService: ContextualHelpService) { }

  ngAfterViewInit() { }

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

  onClickToHelp(helpId) {
    this.helpService.openHelpPage(helpId);
  }

}
