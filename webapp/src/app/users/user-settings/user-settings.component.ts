/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit, ViewChild } from '@angular/core';
import { TranslateService } from '@ngx-translate/core';
import { CredentialFormComponent } from '../credential-form/credential-form.component';
import { TourService } from "../../shared/tour.service";

@Component({
  selector: 'athena-user-settings',
  templateUrl: './user-settings.component.html',
  styleUrls: ['./user-settings.component.scss']
})
export class UserSettingsComponent implements OnInit {
  @ViewChild('authenticationForm') authenticationForm: CredentialFormComponent;

  isOpen = false;
  organizations: any[] = [
    'Bank of China',
    'Bank of America',
    'Bank of London'
  ];

  constructor(private translate: TranslateService, private tourService: TourService) {
  }

  ngOnInit() {
  }

  editSettings() {
    alert(this.translate.instant('users.settings.editSettings.alertMessage'));
  }

  openDownloadCertificationWizard() {
    this.isOpen = true;
  }

  downloadWizardClosed(isOpen: boolean) {
    this.isOpen = isOpen;
  }

  onPrev() {
    this.tourService.toggleUserProfileMenu();
  }


}
