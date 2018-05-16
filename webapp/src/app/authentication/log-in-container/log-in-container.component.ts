/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component } from '@angular/core';
import { Router } from '@angular/router';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';

import { AuthenticationService } from '../../shared/authentication.service';

@Component({
  selector: 'app-log-in-container',
  templateUrl: './log-in-container.component.html',
  styleUrls: ['./log-in-container.component.scss']
})
export class LogInContainerComponent {

  readonly loginForm: FormGroup;
  private authenticationChange;

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
      password: ['', [Validators.required]]
    });
  }

  onLogIn() {
    this.authenticationService.logIn(this.loginForm.value.email, 'password');
  }
}
