/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit, ViewChild, ElementRef } from '@angular/core';
import { Router } from '@angular/router';
import { AuthenticationService } from '../../shared/authentication.service';

@Component({
  selector: 'athena-onboarding',
  templateUrl: './onboarding.component.html',
  styleUrls: ['./onboarding.component.scss']
})
export class OnboardingComponent implements OnInit {
  @ViewChild('agreementEl') agreementEl: ElementRef;
  agreement: {type?: string, content?: string, accepted: boolean, id?: number};
  disabledAgreement = true;

  constructor(
    private authService: AuthenticationService,
    private router: Router,
   ) {
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
    this.authService.acceptLegalAgreement()
      .subscribe(response => {
        this.authService.agreement.accepted = true;
        this.goToLogin();
        return response;
      });
  }

  private handleAgreement(agreement) {
    this.agreement = agreement;
    if (this.agreement.accepted) {
      this.goToLogin();
    }
  }

  private scrollHandler(event) {
    const bottom = event.srcElement.scrollHeight - 492;
    const reachedBottom = event.srcElement.scrollTop === bottom;
    if (reachedBottom && this.disabledAgreement) {
      this.disabledAgreement = false;
    }
  }

  private goToLogin() {
    this.router.navigate(['auth', 'login']);
  }


}
