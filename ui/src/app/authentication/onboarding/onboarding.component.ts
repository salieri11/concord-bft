/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, OnInit, ViewChild, ElementRef } from '@angular/core';
import { Router } from '@angular/router';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';

import { AuthenticationService } from '../../shared/authentication.service';

@Component({
  selector: 'concord-onboarding',
  templateUrl: './onboarding.component.html',
  styleUrls: ['./onboarding.component.scss']
})
export class OnboardingComponent implements OnInit {
  @ViewChild('agreementEl', { static: true }) agreementEl: ElementRef;
  agreement: { type?: string, content?: string, accepted: boolean, id?: number };
  disabledAgreement = true;
  agreementForm: FormGroup;
  showServiceAvailability: boolean = false;

  constructor(
    private authService: AuthenticationService,
    private router: Router,
    private fb: FormBuilder,
  ) {
    this.agreementForm = this.fb.group({
      firstName: ['', Validators.required],
      lastName: ['', Validators.required],
      company: ['', Validators.required],
    });
  }

  ngOnInit() {
    this.agreement = this.authService.agreement;

    if (!this.agreement.content) {
      this.authService.checkForLegalAgreements()
        .subscribe(agreement => this.handleAgreement(agreement));
    } else {
      this.handleAgreement(this.agreement);
    }

    this.agreementEl.nativeElement
      .addEventListener('scroll', this.scrollHandler.bind(this));
  }

  accept(): void {
    this.authService.acceptLegalAgreement({
      first_name: this.agreementForm.value.firstName,
      last_name: this.agreementForm.value.lastName,
      company: this.agreementForm.value.company,
      accepted: true,
    })
      .subscribe(response => {
        this.authService.agreement.accepted = true;
        this.goToServiceAvailablity();
        return response;
      });
  }

  next(): void {
    this.goToLogin();
  }

  private handleAgreement(agreement) {
    this.agreement = agreement;
    if (this.agreement.accepted) {
      this.goToLogin();
    }
  }

  private scrollHandler(event) {
    const el = event.target || event.srcElement;
    const bottom = el.scrollHeight - el.offsetHeight - 10;
    const reachedBottom = el.scrollTop >= bottom;

    if (reachedBottom && this.disabledAgreement) {
      this.disabledAgreement = false;
    }
  }

  private goToServiceAvailablity() {
    this.showServiceAvailability = true;
  }

  private goToLogin() {
    this.router.navigate(['auth', 'login']);
  }


}
