/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnDestroy } from '@angular/core';
import { Router } from '@angular/router';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';

import { AuthenticationService } from '../../shared/authentication.service';
import { Personas } from '../../shared/persona.service';

@Component({
  selector: 'athena-log-in-container',
  templateUrl: './log-in-container.component.html',
  styleUrls: ['./log-in-container.component.scss']
})
export class LogInContainerComponent implements OnDestroy {

  readonly loginForm: FormGroup;
  private authenticationChange;
  personaOptions: Array<{ name ?: string; value: string; }> = [
    { value: Personas.SystemsAdmin, name: 'Systems Admin' },
    { value: Personas.ConsortiumAdmin, name: 'Consortium Admin' },
    { value: Personas.OrgAdmin, name: 'Org Admin' },
    { value: Personas.OrgDeveloper, name: 'Org Developer' },
    { value: Personas.OrgUser, name: 'Org User' },
  ];

  constructor(private authenticationService: AuthenticationService,
              private formBuilder: FormBuilder,
              private router: Router) {
    this.authenticationChange = this.authenticationService.user.subscribe(email => {
      if (email) {
        this.router.navigate(['/dashboard']);
      }
    });

    this.loginForm = this.formBuilder.group({
      email: ['', [Validators.required]],
      password: ['', [Validators.required]],
      persona: [this.personaOptions[0].value]
    });
  }

  ngOnDestroy () {
    this.authenticationChange.unsubscribe();
  }

  onLogIn() {
    this.authenticationService.logIn(this.loginForm.value.email, 'password', this.loginForm.value.persona);
  }
}
