/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit, ViewChild } from '@angular/core';
import { TranslateService } from '@ngx-translate/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { ClrWizard } from '@clr/angular';

@Component({
  selector: 'athena-user-settings',
  templateUrl: './user-settings.component.html',
  styleUrls: ['./user-settings.component.scss']
})
export class UserSettingsComponent implements OnInit {
  @ViewChild('wizard') wizard: ClrWizard;
  @ViewChild('myForm') myForm: any;

  isOpen = false;
  downloadCertificateForm: FormGroup;

  constructor(private translate: TranslateService) {
    this.initializeDownloadWizard();
  }

  ngOnInit() {
  }

  editSettings() {
    alert(this.translate.instant('users.settings.editSettings.alertMessage'));
  }

  openDownloadCertificationWizard() {
    this.isOpen = true;
  }

  initializeDownloadWizard() {
    this.downloadCertificateForm = new FormGroup({
      password: new FormControl('', Validators.required),
      token: new FormControl('', Validators.required),
    });
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
    // this.onDownload(sourceFile, `${this.generateFilename()}`);
  }

  // Code to download certificate

  // private onDownload(source, file) {
  //   const a: HTMLAnchorElement = document.createElement('a');
  //   document.body.appendChild(a);
  //   a.style.display = 'none';
  //
  //   const blob = new Blob([source], { type: 'octet/stream' });
  //   const url = window.URL.createObjectURL(blob);
  //   a.href = url;
  //   a.download = file;
  //   a.click();
  //   window.URL.revokeObjectURL(url);
  // }
  //
  // private generateFilename() {
  //   return 'Certificate';
  // }

}
