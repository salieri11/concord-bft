/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, OnInit, ViewChild, ElementRef } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';

import { AuthenticationService } from '../../shared/authentication.service';
import { mainFragments } from '../../shared/urls.model';
import { RouteService } from '../../shared/route.service';

@Component({
  selector: 'concord-onboarding',
  templateUrl: './onboarding.component.html',
  styleUrls: ['./onboarding.component.scss']
})
export class OnboardingComponent implements OnInit {
  @ViewChild('agreementEl', { static: true }) agreementEl: ElementRef;
  agreement:  boolean;
  disabledAgreement = true;
  agreementForm: FormGroup;
  showServiceAvailability: boolean = false;

  constructor(
    private authService: AuthenticationService,
    private fb: FormBuilder,
    private routeService: RouteService,
  ) {
    this.agreementForm = this.fb.group({
      firstName: ['', Validators.required],
      lastName: ['', Validators.required],
      company: ['', Validators.required],
    });
  }

  ngOnInit() {
    this.authService.checkForLegalAgreements()
      .subscribe(agreement => this.handleAgreement(agreement));

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
        this.authService.agreement = true;
        this.goToWelcome();
        return response;
      });
  }

  private handleAgreement(agreement: boolean) {
    if (agreement) {
      this.goToWelcome();
    } else {
      this.authService.getLegalAgreement().subscribe(agr => {
        this.agreement = agr;
      });
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

  private async goToWelcome() {
    await this.routeService.resolveConsortium();
    this.routeService.redirectToDefault(mainFragments.welcome);
  }


}
