/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import {
  Component,
  OnInit,
  ViewChild,
  Output,
  EventEmitter,
  ElementRef
} from '@angular/core';
import { Router } from '@angular/router';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { ClrWizard, ClrWizardPage } from '@clr/angular';
import { AuthenticationService } from '../../authentication.service';
import { PersonaService } from '../../persona.service';
import { BlockchainService, BlockchainRequestParams } from '../../blockchain.service';

@Component({
  selector: 'concord-blockchain-wizard',
  templateUrl: './blockchain-wizard.component.html',
  styleUrls: ['./blockchain-wizard.component.scss']
})
export class BlockchainWizardComponent implements OnInit {
  @ViewChild('wizard') wizard: ClrWizard;
  @ViewChild('detailPage') blockConsortiumPage: ClrWizardPage;
  @ViewChild('usersPage') usersPage: ClrWizardPage;
  @ViewChild('consortiumInput') consortiumInput: ElementRef;
  @Output('setupComplete') setupComplete: EventEmitter<any> = new EventEmitter<any>();

  isOpen = false;
  form: FormGroup;
  userForm: FormGroup;

  personaOptions = PersonaService.getOptions();
  numbersOfNodes = [4, 7];
  fCountMapping = {'4': 1, '7': 2};

  constructor(
    private router: Router,
    private authenticationService: AuthenticationService,
    private blockchainService: BlockchainService
  ) {
    this.form = new FormGroup({
      details: new FormGroup({
        name: new FormControl('', Validators.required),
        description: new FormControl('', Validators.required),
        numberOfNodes: new FormControl(this.numbersOfNodes[1], Validators.required)
      }),
      users: new FormControl([], Validators.required)
    });
    this.userForm = new FormGroup({
      email: new FormControl('', [Validators.required, Validators.email]),
      role: new FormControl('', Validators.required)
    });
  }

  ngOnInit() {
  }

  addUser() {
    const selectedUsers = this.form.get('users');
    selectedUsers.setValue(selectedUsers.value.concat([this.userForm.value]));
    this.userForm.reset();
  }

  resetFragment() {
    this.router.navigate([]);
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

    setTimeout( () => {
      this.consortiumInput.nativeElement.focus();
    }, 1000);
  }

  onSubmit() {
    const params = new BlockchainRequestParams();
    params.f_count = Number(this.fCountMapping[this.form.value.details.numberOfNodes.toString()]);
    params.consortium_id = this.authenticationService.currentUser.consortium_id;

    this.blockchainService.notify.next({message: 'deploying'});
    this.blockchainService.deploy(params).subscribe(response => {
      this.setupComplete.emit(response);
    }, error => {
      this.setupComplete.emit(error);
    });
  }

  close() {
    this.isOpen = false;
  }
}
