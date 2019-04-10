/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, OnInit, ViewChild, Output, EventEmitter } from '@angular/core';
import { Router } from '@angular/router';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { ClrWizard, ClrWizardPage } from '@clr/angular';
import { validateNumberOfNodes } from '../../custom-validators';
import { PersonaService } from '../../persona.service';

@Component({
  selector: 'concord-blockchain-wizard',
  templateUrl: './blockchain-wizard.component.html',
  styleUrls: ['./blockchain-wizard.component.scss']
})
export class BlockchainWizardComponent implements OnInit {
  @ViewChild('wizard') wizard: ClrWizard;
  @ViewChild('detailPage') blockConsortiumPage: ClrWizardPage;
  @ViewChild('usersPage') usersPage: ClrWizardPage;
  @Output('setupComplete') setupComplete: EventEmitter<any> = new EventEmitter<any>();

  isOpen = false;
  form: FormGroup;
  userForm: FormGroup;

  personaOptions = PersonaService.getOptions();

  constructor(private router: Router) {
    this.form = new FormGroup({
      details: new FormGroup({
        name: new FormControl('', Validators.required),
        description: new FormControl('', Validators.required),
        numberOfNodes: new FormControl('', [Validators.required, validateNumberOfNodes()])
      }),
      users: new FormControl([], Validators.required)
    });

    this.userForm = new FormGroup({
      email: new FormControl('', [Validators.required, Validators.email]),
      role: new FormControl('', Validators.required)
    });
  }

  ngOnInit() {}

  addUser() {
    const selectedUsers = this.form.get('users');
    selectedUsers.setValue(selectedUsers.value.concat([this.userForm.value]));
    this.userForm.reset();
  }

  resetFragment() {
    this.router.navigate(['/dashboard']);
  }

  deleteUser(index: number) {
    const selectedUsers = this.form.get('users');

    // @ts-ignore: no unused locals
    selectedUsers.setValue(selectedUsers.value.filter((item, i) => {
      return index !== i;
    }));
  }

  personaName(value): string {
    return PersonaService.getName(value);
  }

  open() {
    this.isOpen = true;
    this.wizard.reset();
    this.form.reset();
    this.form.patchValue({
      users: []
    });
    this.userForm.reset();
  }

  onSubmit() {
    this.setupComplete.emit(this.form.value);
    this.resetFragment();
  }
}
