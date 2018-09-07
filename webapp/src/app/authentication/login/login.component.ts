/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnDestroy, ViewChild, ElementRef, AfterViewInit } from '@angular/core';
import { Router, NavigationExtras } from '@angular/router';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { TranslateService } from '@ngx-translate/core';

import { AuthenticationService } from '../../shared/authentication.service';
import { PersonaService } from '../../shared/persona.service';

@Component({
  selector: 'athena-login',
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.scss']
})
export class LogInContainerComponent implements OnDestroy, AfterViewInit {
  @ViewChild('username') username: ElementRef;
  readonly loginForm: FormGroup;
  errorMessage: string;
  personaOptions = PersonaService.getOptions();

  constructor(
    private authenticationService: AuthenticationService,
    private formBuilder: FormBuilder,
    private router: Router,
    private translateService: TranslateService
  ) {

    this.loginForm = this.formBuilder.group({
      email: ['', [Validators.required]],
      password: ['', [Validators.required]],
      persona: [this.personaOptions[0].value]
    });
  }

  ngAfterViewInit() {
    this.username.nativeElement.focus();
  }

  ngOnDestroy() {}

  onLogIn() {
    this.errorMessage = null;
    this.authenticationService.logIn(
      this.loginForm.value.email,
      this.loginForm.value.password,
      this.loginForm.value.persona
    ).subscribe((user) => {
      if (user['last_login'] === 0) {
        const navExtras: NavigationExtras = {
          fragment: 'orgTour'
        };
        this.router.navigate(['/dashboard'], navExtras);
      } else {
        this.router.navigate(['dashboard']);
      }
    }, (error) => {
      this.errorMessage = error.error.error || this.translateService.instant('authentication.errorMessage');
    });
  }
}
