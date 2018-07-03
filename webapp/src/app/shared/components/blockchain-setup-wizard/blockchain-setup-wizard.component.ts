/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit, ViewChild, Output, EventEmitter } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { ClrWizard } from '@clr/angular';

@Component({
  selector: 'athena-blockchain-setup-wizard',
  templateUrl: './blockchain-setup-wizard.component.html',
  styleUrls: ['./blockchain-setup-wizard.component.scss']
})
export class BlockchainSetupWizardComponent implements OnInit {
  @ViewChild('wizard') wizard: ClrWizard;
  @Output('setupComplete') setupComplete: EventEmitter<any> = new EventEmitter<any>();

  isOpen = false;
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

  constructor() {
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
    selectedOrgs.setValue(selectedOrgs.value.concat([this.orgForm.value]));

    this.orgForm.reset();
  }

  addUser() {
    let selectedUsers= this.form.get('users');
    selectedUsers.setValue(selectedUsers.value.concat([this.userForm.value]));

    this.userForm.reset();
  }

  open() {
    this.isOpen = true;
    this.wizard.reset();
    this.form.reset();
    this.form.patchValue({
      organizations: [],
      users: []
    })
  }

  onSubmit() {
    this.setupComplete.emit(this.form.value);
  }

}
