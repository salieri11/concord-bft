/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';
import { FormGroup, FormControl, Validators } from '@angular/forms';

import { of } from 'rxjs';
import { delay } from 'rxjs/operators';

@Component({
  selector: 'concord-inivte-user',
  templateUrl: './inivte-user.component.html',
  styleUrls: ['./inivte-user.component.scss']
})
export class InivteUserComponent implements OnInit {
  isOpen: boolean;
  inviting: boolean;
  form: FormGroup;
  inviteSent: boolean;

  constructor() {
    this.form = new FormGroup({
      email: new FormControl('', [Validators.required, Validators.email])
    });
  }

  ngOnInit() {
  }

  open(): void {
    this.isOpen = true;
  }

  invite() {
    this.inviting = true;
    this.inviteSent = false;

    of(true).pipe(delay(2000)).subscribe(() => {
      this.inviting = false;
      this.inviteSent = true;
      this.form.reset();
    });
  }

}
