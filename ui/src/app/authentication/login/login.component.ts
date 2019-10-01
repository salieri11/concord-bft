/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, OnDestroy, ViewChild, ElementRef, AfterViewInit } from '@angular/core';
import { Router, NavigationExtras } from '@angular/router';
import { FormBuilder, FormGroup, Validators, } from '@angular/forms';
import { TranslateService } from '@ngx-translate/core';

import { AuthenticationService } from '../../shared/authentication.service';
import { PersonaService } from '../../shared/persona.service';
import { matchPasswordValidator } from '../../shared/custom-validators';
import { BlockchainService } from '../../blockchain/shared/blockchain.service';


@Component({
  selector: 'concord-login',
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.scss']
})
export class LogInContainerComponent implements OnDestroy, AfterViewInit {
  @ViewChild('username', { static: true }) username: ElementRef;
  @ViewChild('newPassword', { static: true }) newPassword: ElementRef;
  readonly loginForm: FormGroup;
  readonly changePasswordForm: FormGroup;
  errorMessage: string;
  personaOptions = PersonaService.getOptions();
  hideLoginForm: boolean = false;
  hideChangePassword: boolean = true;
  newUser: boolean = false;
  passwordTest: RegExp = /^(?=.*\d)(?=.*[a-z])(?=.*[A-Z])(?=.*[^a-zA-Z0-9])(?!.*\s).{6,35}$/;

  constructor(
    private authenticationService: AuthenticationService,
    private formBuilder: FormBuilder,
    private router: Router,
    private translateService: TranslateService,
    private blockchainService: BlockchainService,
  ) {

    this.loginForm = this.formBuilder.group({
      email: ['', [Validators.required]],
      password: ['', [Validators.required]],
      persona: [this.personaOptions[0].value]
    });

    this.changePasswordForm = this.formBuilder.group({
      newPassword: ['', [
        Validators.required,
        Validators.pattern(this.passwordTest)
      ]],
      confirmPassword: ['', [
        Validators.required,
        matchPasswordValidator('newPassword')
      ]],
    });

  }

  ngAfterViewInit() {
    this.username.nativeElement.focus();
  }

  ngOnDestroy() { }

  onLogIn() {
    this.errorMessage = null;
    this.authenticationService.logIn(
      this.loginForm.value.email,
      this.loginForm.value.password,
      this.loginForm.value.persona
    ).subscribe(
      user => this.handleLogin(user),
      (error) => {
        if (error) {
          this.errorMessage = this.translateService.instant('authentication.errorMessage');
        }
      });
  }

  private handleLogin(user) {
    const redirectUrl = this.authenticationService.redirectUrl;

    this.blockchainService.set().subscribe(() => {
      const blockchain = this.blockchainService.blockchainId;

      if (user['last_login'] === 0 || localStorage.getItem('changePassword')) {
        localStorage.setItem('changePassword', 'true');
        this.newUser = true;
        this.hideLoginForm = true;
        this.hideChangePassword = false;
        setTimeout(() => {
          this.newPassword.nativeElement.focus();
        }, 10);
      } else {
        if (redirectUrl) {
          this.router.navigateByUrl(redirectUrl);
        } else if (!blockchain) {
          this.router.navigate(['welcome'], {fragment: 'welcome'});
        } else {
          this.router.navigate([blockchain, 'dashboard']);
        }
      }
    });
  }

  changePassword() {
    const blockchain = this.blockchainService.blockchainId;

    this.errorMessage = null;
    this.authenticationService.changePassword(
      this.loginForm.value.email,
      this.changePasswordForm.value.newPassword,
    ).subscribe((response) => {
      if (response && this.newUser) {
        localStorage.removeItem('changePassword');

        const navExtras: NavigationExtras = {
          fragment: 'welcome'
        };
        this.router.navigate(['welcome'], navExtras);
      } else {
        this.router.navigate([blockchain, 'dashboard']);
      }
    }, (error) => {
      this.errorMessage = error.error.error || this.translateService.instant('authentication.errorMessage');
    });
  }


}
