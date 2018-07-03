/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit, ViewChild, Output, EventEmitter } from '@angular/core';
import { FormControl, FormGroup, FormArray } from '@angular/forms';
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
        type: new FormControl('')
      }),
      faultTolerance: new FormGroup({
        type: new FormControl('')
      }),
      consortium: new FormGroup({
        name: new FormControl('')
      }),
      organizations: new FormGroup({
        name: new FormControl(''),
        selectedOrganizations: new FormArray([
          new FormControl(null)
        ])
      }),
      users: new FormGroup({
        firstName: new FormControl(''),
        lastName: new FormControl(''),
        email: new FormControl(''),
        organization: new FormControl(''),
        role: new FormControl(''),
        selectedUsers: new FormArray([
          new FormControl(null)
        ])
      }),
      advancedSettings: new FormGroup({
        networkName: new FormControl(''),
        numberOfNodes: new FormControl(),
        publicNodesRegions: new FormControl(''),
        privateNode: new FormControl('')
      })
    });
  }

  ngOnInit() {}

  open() {
    this.isOpen = true;
    this.wizard.reset();
    this.form.reset();
  }

  onSubmit() {
    this.setupComplete.emit(this.form.value);
  }

}
