/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit, ViewChild, Output, EventEmitter } from '@angular/core';
import { Router } from '@angular/router';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { ClrWizard, ClrWizardPage } from '@clr/angular';

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
    {value: 'london-datacenter', displayValue: 'london-datacenter'},
    {value: 'us-datacenter', displayValue: 'us-datacenter'},
  ];
  publicNodeItems = [
    {value: 'aws-south-asia', displayValue: 'AWS South Asia'},
    {value: 'aws-east-asia', displayValue: 'AWS East Asia'},
    {value: 'aws-north-america', displayValue: 'AWS North America'},
    {value: 'aws-south-america', displayValue: 'AWS South America'},
    {value: 'aws-western-europe', displayValue: 'AWS Western Europe'},
    {value: 'aws-eastern-europe', displayValue: 'AWS Eastern Europe'},
    {value: 'aws-africa', displayValue: 'AWS Africa'},
    {value: 'aws-australia', displayValue: 'AWS Australia'}
  ];

  constructor(private router: Router) {
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
    let selectedOrgs = this.form.get('organizations');

    if(this.isEditingOrg) {
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
    let selectedUsers= this.form.get('users');

    if(this.isEditingUser) {
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
    let selectedOrgs = this.form.get('organizations');

    // @ts-ignore: no unused locals
    selectedOrgs.setValue(selectedOrgs.value.filter((item, i) => {
      return index !== i;
    }));
  }

  onEditUser(index: nubmer) {
    this.userIndex = index;
    this.isEditingUser = true;
    this.userForm.patchValue(this.form.get('users').value[index]);
  }

  deleteUser(index: number) {
    let selectedUsers = this.form.get('users');

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
