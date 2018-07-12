/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit, ViewChild, Output, EventEmitter } from '@angular/core';
import { Router } from '@angular/router';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { ClrWizard, ClrWizardPage } from '@clr/angular';
import { TranslateService } from '@ngx-translate/core';

import { Personas } from '../../persona.service';

@Component({
  selector: 'athena-blockchain-wizard',
  templateUrl: './blockchain-wizard.component.html',
  styleUrls: ['./blockchain-wizard.component.scss']
})
export class BlockchainWizardComponent implements OnInit {
  @ViewChild('wizard') wizard: ClrWizard;
  @ViewChild('blockConsortiumPage') blockConsortiumPage: ClrWizardPage;
  @ViewChild('orgsPage') orgsPage: ClrWizardPage;
  @ViewChild('usersPage') usersPage: ClrWizardPage;
  @Output('setupComplete') setupComplete: EventEmitter<any> = new EventEmitter<any>();

  isOpen = false;
  form: FormGroup;
  orgForm: FormGroup;
  userForm: FormGroup;
  locationOptions = [
    { value: 'sydney', name: 'blockchainWizard.locations.sydney' },
    { value: 'vienna', name: 'blockchainWizard.locations.vienna' },
    { value: 'brussels', name: 'blockchainWizard.locations.brussels' },
    { value: 'rioDeJaneiro', name: 'blockchainWizard.locations.rioDeJaneiro' },
    { value: 'toronto', name: 'blockchainWizard.locations.toronto' },
    { value: 'santiago', name: 'blockchainWizard.locations.santiago' },
    { value: 'beijing', name: 'blockchainWizard.locations.beijing' },
    { value: 'cairo', name: 'blockchainWizard.locations.cairo' },
    { value: 'paris', name: 'blockchainWizard.locations.paris' },
    { value: 'delhi', name: 'blockchainWizard.locations.delhi' },
    { value: 'milan', name: 'blockchainWizard.locations.milan' },
    { value: 'tokyo', name: 'blockchainWizard.locations.tokyo' },
    { value: 'mexicoCity', name: 'blockchainWizard.locations.mexicoCity' },
    { value: 'moscow', name: 'blockchainWizard.locations.moscow' },
    { value: 'seoul', name: 'blockchainWizard.locations.seoul' },
    { value: 'johannesburg', name: 'blockchainWizard.locations.johannesburg' },
    { value: 'london', name: 'blockchainWizard.locations.london' },
    { value: 'lasVegas', name: 'blockchainWizard.locations.lasVegas' },
    { value: 'newYork', name: 'blockchainWizard.locations.newYork' },
    { value: 'paloAlto', name: 'blockchainWizard.locations.paloAlto' },
  ];
  personaOptions: Array<{ name ?: string; value: Personas; }> = [
    { value: Personas.SystemsAdmin, name: 'personas.systemsAdmin' },
    { value: Personas.ConsortiumAdmin, name: 'personas.consortiumAdmin' },
    { value: Personas.OrgAdmin, name: 'personas.orgAdmin' },
    { value: Personas.OrgDeveloper, name: 'personas.orgDeveloper' },
    { value: Personas.OrgUser, name: 'personas.orgUser' }
  ];
  privateNodeItems = [
    {
      value: 'london-datacenter',
      displayValue: this.translateService.instant('blockchainWizard.reviewDeployment.privateNodeValues.londonDatacenter')
    },
    {
      value: 'us-datacenter',
      displayValue: this.translateService.instant('blockchainWizard.reviewDeployment.privateNodeValues.usDatacenter')
    },
  ];
  publicNodeItems = [
    {
      value: 'aws-south-asia',
      displayValue: this.translateService.instant('blockchainWizard.reviewDeployment.publicNodeValues.awsSouthAsia')
    },
    {
      value: 'aws-east-asia',
      displayValue: this.translateService.instant('blockchainWizard.reviewDeployment.publicNodeValues.awsEastAsia')
    },
    {
      value: 'aws-north-america',
      displayValue: this.translateService.instant('blockchainWizard.reviewDeployment.publicNodeValues.awsNorthAmerica')
    },
    {
      value: 'aws-south-america',
      displayValue: this.translateService.instant('blockchainWizard.reviewDeployment.publicNodeValues.awsSouthAmerica')
    },
    {
      value: 'aws-western-europe',
      displayValue: this.translateService.instant('blockchainWizard.reviewDeployment.publicNodeValues.awsWesternEurope')
    },
    {
      value: 'aws-eastern-europe',
      displayValue: this.translateService.instant('blockchainWizard.reviewDeployment.publicNodeValues.awsEasternEurope')
    },
    {
      value: 'aws-africa',
      displayValue: this.translateService.instant('blockchainWizard.reviewDeployment.publicNodeValues.awsAfrica')
    },
    {
      value: 'aws-australia',
      displayValue: this.translateService.instant('blockchainWizard.reviewDeployment.publicNodeValues.awsAustralia')
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
      name: new FormControl('', Validators.required),
      location: new FormControl('', Validators.required)
    });

    this.userForm = new FormGroup({
      firstName: new FormControl('', Validators.required),
      lastName: new FormControl('', Validators.required),
      email: new FormControl('', [Validators.required, Validators.email]),
      organization: new FormControl('', Validators.required),
      role: new FormControl('', Validators.required)
    });
  }

  ngOnInit() {}

  addOrg() {
    const selectedOrgs = this.form.get('organizations');

    selectedOrgs.setValue(selectedOrgs.value.concat([this.orgForm.value]));

    this.orgForm.reset();
  }

  addUser() {
    const selectedUsers = this.form.get('users');

    selectedUsers.setValue(selectedUsers.value.concat([this.userForm.value]));

    this.userForm.reset();
  }

  jumpTo(page: ClrWizardPage) {
    this.wizard.navService.setCurrentPage(page);
  }

  resetFragment() {
    this.router.navigate(['/dashboard']);
  }

  deleteOrg(index: number) {
    const selectedOrgs = this.form.get('organizations');

    // @ts-ignore: no unused locals
    selectedOrgs.setValue(selectedOrgs.value.filter((item, i) => {
      return index !== i;
    }));
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
  }

  onSubmit() {
    this.setupComplete.emit(this.form.value);
    this.resetFragment();
  }

}
