/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit, ViewChild, Output, EventEmitter } from '@angular/core';
import { Router } from '@angular/router';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { ClrWizard, ClrWizardPage } from '@clr/angular';
import { TranslateService } from '@ngx-translate/core';

@Component({
  selector: 'athena-blockchain-setup-wizard',
  templateUrl: './blockchain-setup-wizard.component.html',
  styleUrls: ['./blockchain-setup-wizard.component.scss']
})
export class BlockchainSetupWizardComponent implements OnInit {
  @ViewChild('wizard') wizard: ClrWizard;
  @ViewChild('blockConsortiumPage') blockConsortiumPage: ClrWizardPage;
  @ViewChild('orgsPage') orgsPage: ClrWizardPage;
  @ViewChild('usersPage') usersPage: ClrWizardPage;
  @Output('setupComplete') setupComplete: EventEmitter<any> = new EventEmitter<any>();

  isOpen = false;
  isEditingOrg = false;
  orgIndex: number = null;
  isEditingUser = false;
  userIndex: number = null;
  form: FormGroup;
  orgForm: FormGroup;
  userForm: FormGroup;
  privateNodeItems = [
    {
      value: 'london-datacenter',
      displayValue: this.translateService.instant('blockchainSetupWizard.reviewDeployment.privateNodeValues.londonDatacenter')
    },
    {
      value: 'us-datacenter',
      displayValue: this.translateService.instant('blockchainSetupWizard.reviewDeployment.privateNodeValues.usDatacenter')
    },
  ];
  publicNodeItems = [
    {
      value: 'aws-south-asia',
      displayValue: this.translateService.instant('blockchainSetupWizard.reviewDeployment.publicNodeValues.awsSouthAsia')
    },
    {
      value: 'aws-east-asia',
      displayValue: this.translateService.instant('blockchainSetupWizard.reviewDeployment.publicNodeValues.awsEastAsia')
    },
    {
      value: 'aws-north-america',
      displayValue: this.translateService.instant('blockchainSetupWizard.reviewDeployment.publicNodeValues.awsNorthAmerica')
    },
    {
      value: 'aws-south-america',
      displayValue: this.translateService.instant('blockchainSetupWizard.reviewDeployment.publicNodeValues.awsSouthAmerica')
    },
    {
      value: 'aws-western-europe',
      displayValue: this.translateService.instant('blockchainSetupWizard.reviewDeployment.publicNodeValues.awsWesternEurope')
    },
    {
      value: 'aws-eastern-europe',
      displayValue: this.translateService.instant('blockchainSetupWizard.reviewDeployment.publicNodeValues.awsEasternEurope')
    },
    {
      value: 'aws-africa',
      displayValue: this.translateService.instant('blockchainSetupWizard.reviewDeployment.publicNodeValues.awsAfrica')
    },
    {
      value: 'aws-australia',
      displayValue: this.translateService.instant('blockchainSetupWizard.reviewDeployment.publicNodeValues.awsAustralia')
    }
  ];

  constructor(private router: Router, private translateService: TranslateService) {
    this.form = new FormGroup({
      blockchain: new FormGroup({
        type: new FormControl('', Validators.required)
      }),
      faultTolerance: new FormGroup({
        type: new FormControl('', Validators.required)
      }),
      consortium: new FormGroup({
        name: new FormControl('', Validators.required)
      }),
      organizations: new FormControl([], Validators.required),
      users: new FormControl([], Validators.required),
      advancedSettings: new FormGroup({
        networkName: new FormControl(''),
        numberOfNodes: new FormControl(null),
        publicNodesRegions: new FormControl(''),
        privateNode: new FormControl('')
      })
    });

    this.orgForm = new FormGroup({
      name: new FormControl('', Validators.required)
    });

    this.userForm = new FormGroup({
      firstName: new FormControl('', Validators.required),
      lastName: new FormControl('', Validators.required),
      email: new FormControl('', Validators.required),
      organization: new FormControl('', Validators.required),
      role: new FormControl('', Validators.required)
    });
  }

  ngOnInit() {}

  addOrg() {
    const selectedOrgs = this.form.get('organizations');

    if (this.isEditingOrg) {
      selectedOrgs.value[this.orgIndex] = this.orgForm.value;
      selectedOrgs.setValue(selectedOrgs.value);
      this.isEditingOrg = false;
      this.orgIndex = null;
    } else {
      selectedOrgs.setValue(selectedOrgs.value.concat([this.orgForm.value]));
    }

    this.orgForm.reset();
  }

  addUser() {
    const selectedUsers = this.form.get('users');

    if (this.isEditingUser) {
      selectedUsers.value[this.userIndex] = this.userForm.value;
      selectedUsers.setValue(selectedUsers.value);
      this.isEditingUser = false;
      this.userIndex = null;
    } else {
      selectedUsers.setValue(selectedUsers.value.concat([this.userForm.value]));
    }

    this.userForm.reset();
  }

  jumpTo(page: ClrWizardPage) {
    this.wizard.navService.setCurrentPage(page);
  }

  resetFragment() {
    this.router.navigate(['/dashboard']);
  }

  onEditOrg(index: number) {
    this.orgIndex = index;
    this.isEditingOrg = true;
    this.orgForm.patchValue(this.form.get('organizations').value[index]);
  }

  deleteOrg(index: number) {
    const selectedOrgs = this.form.get('organizations');

    // @ts-ignore: no unused locals
    selectedOrgs.setValue(selectedOrgs.value.filter((item, i) => {
      return index !== i;
    }));
  }

  onEditUser(index: number) {
    this.userIndex = index;
    this.isEditingUser = true;
    this.userForm.patchValue(this.form.get('users').value[index]);
  }

  deleteUser(index: number) {
    const selectedUsers = this.form.get('users');

    // @ts-ignore: no unused locals
    selectedUsers.setValue(selectedUsers.value.filter((item, i) => {
      return index !== i;
    }));
  }

  open() {
    this.isOpen = true;
    this.wizard.reset();
    this.form.reset();
    this.form.patchValue({
      organizations: [],
      users: []
    });
    this.userForm.reset();
    this.orgForm.reset();
    this.isEditingOrg = false;
    this.isEditingUser = false;
  }

  onSubmit() {
    this.setupComplete.emit(this.form.value);
    this.resetFragment();
  }

}
