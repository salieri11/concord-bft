/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnDestroy, ViewChild, ElementRef, AfterViewInit, NgZone } from '@angular/core';
import { Router } from '@angular/router';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';

import { AuthenticationService } from '../../shared/authentication.service';
import { PersonaService } from '../../shared/persona.service';
import { ErrorAlertService } from '../../shared/global-error-handler.service';

@Component({
  selector: 'athena-log-in-container',
  templateUrl: './log-in-container.component.html',
  styleUrls: ['./log-in-container.component.scss']
})
export class LogInContainerComponent implements OnDestroy, AfterViewInit {
  @ViewChild('username') username: ElementRef;
  readonly loginForm: FormGroup;
  private authenticationChange;
  alerts: any = [];
  personaOptions = PersonaService.getOptions();

  constructor(
    private authenticationService: AuthenticationService,
    private formBuilder: FormBuilder,
    private alertService: ErrorAlertService,
    private router: Router,
    private zone: NgZone
  ) {

    this.authenticationChange = this.authenticationService.user.subscribe(user => {
      if (user.email) {
        this.router.navigate(['dashboard']);
      }
    });

    this.loginForm = this.formBuilder.group({
      email: ['', [Validators.required]],
      password: ['', [Validators.required]],
      persona: [this.personaOptions[0].value]
    });

    this.alertService.notify
      .subscribe(error => this.addAlert(error));
  }

  ngAfterViewInit() {
    this.username.nativeElement.focus();
  }

  ngOnDestroy () {
    this.authenticationChange.unsubscribe();
  }

  onLogIn() {
    this.authenticationService.logIn(this.loginForm.value.email, this.loginForm.value.password).subscribe(() => {
      this.authenticationService.onLogIn(this.loginForm.value.email, this.loginForm.value.password, this.loginForm.value.persona);
    }, (error) => {
      this.alertService.add(error);
    });
  }

  private addAlert(alert: any): void {
    if (alert && alert.message) {
      const alertItem = {
        message: alert.message
      };
      if (this.alerts.indexOf(alertItem) === -1) {
        this.zone.run(() => this.alerts.push(alertItem));
      }
    }
  }
}
