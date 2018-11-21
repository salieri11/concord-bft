/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';
import { FormGroup, FormBuilder, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { TranslateService } from '@ngx-translate/core';

import { AuthenticationService } from '../../shared/authentication.service';
import { Personas } from '../../shared/persona.service';
import { UsersService } from '../../users/shared/users.service';

@Component({
  selector: 'concord-sign-up',
  templateUrl: './sign-up.component.html',
  styleUrls: ['./sign-up.component.scss']
})
export class SignUpComponent implements OnInit {
  errorMessage: string;
  signupForm: FormGroup;
  countryList: Array<string>;

  constructor(
    private formBuilder: FormBuilder,
    private router: Router,
    private authenticationService: AuthenticationService,
    private usersService: UsersService,
    private translateService: TranslateService
  ) {

    this.signupForm = this.formBuilder.group({
      firstName: ['', [Validators.required]],
      lastName: ['', [Validators.required]],
      email: ['', [Validators.required, Validators.email]],
      password: ['', [Validators.required, Validators.minLength(8)]],
      company: ['', [Validators.required]],
      jobTitle: [''],
      country: ['', [Validators.required]],
      relationship: ['', [Validators.required]],
      numberOfEmployees: ['', [Validators.required]],
      phoneNumber: [''],
    });

    this.countryList = this.authenticationService.getCountryList();
  }

  ngOnInit() { }

  signUp() {
    this.errorMessage = null;
    this.usersService.createUser({
      name: `${this.signupForm.value.firstName} ${this.signupForm.value.lastName}`,
      email: this.signupForm.value.email,
      password: this.signupForm.value.password,
      role: Personas.SystemsAdmin,
      details: {
        first_name: this.signupForm.value.firstName,
        last_name: this.signupForm.value.lastName,
      }
    }).subscribe(() => {
      this.authenticationService.logIn(this.signupForm.value.email, this.signupForm.value.password, Personas.SystemsAdmin).subscribe(() => {
        this.router.navigate(['auth', 'onboarding']);
      });
    }, (error) => {
      if (error.error.error === 'Duplicate email address') {
        this.errorMessage = this.translateService.instant('signUp.duplicateEmailError');
      }
    });
  }

}
