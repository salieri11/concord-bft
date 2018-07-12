/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, EventEmitter, Input, OnInit, Output, ViewChild } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { ClrWizard } from '@clr/angular';

@Component({
  selector: 'athena-authentication-form',
  templateUrl: './authentication-form.component.html',
  styleUrls: ['./authentication-form.component.scss']
})
export class AuthenticationFormComponent implements OnInit {
  @Input('isOpen') isOpen: boolean;
  @Output('closed') closed: EventEmitter<any> = new EventEmitter<any>();
  @ViewChild('wizard') wizard: ClrWizard;

  downloadCertificateForm: FormGroup;

  constructor() {
    this.downloadCertificateForm = new FormGroup({
      password: new FormControl('', Validators.required),
      token: new FormControl('', Validators.required),
    });
  }

  ngOnInit() {
  }

  get passwordSelectNextDisabled() {
    return this.downloadCertificateForm.get('password').invalid;
  }

  get tokenSelectNextDisabled() {
    return this.downloadCertificateForm.get('token').invalid;
  }

  doCancel() {
    this.wizard.close();
    this.downloadCertificateForm.reset();
    this.wizard.reset();
    this.closed.emit(this.isOpen);
  }

  onNext() {
    this.wizard.next();
  }

  goBack() {
    this.wizard.previous();
  }

  doCustomClick(buttonType: string) {
    if ('download-btn' === buttonType) {
      this.downloadCertificate();
    }
  }


  downloadCertificate() {
  }

}
