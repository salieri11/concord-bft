/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnDestroy } from '@angular/core';
import { Router } from '@angular/router';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';

import { AuthenticationService } from '../../shared/authentication.service';
import { PersonaService } from "../../shared/persona.service";

@Component({
  selector: 'athena-log-in-container',
  templateUrl: './log-in-container.component.html',
  styleUrls: ['./log-in-container.component.scss']
})
export class LogInContainerComponent implements OnDestroy {

  readonly loginForm: FormGroup;
  private authenticationChange;
  personaOptions = PersonaService.getOptions();

  constructor(private authenticationService: AuthenticationService,
              private formBuilder: FormBuilder,
              private router: Router) {
    this.authenticationChange = this.authenticationService.user.subscribe(user => {
      if (user.email) {
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
