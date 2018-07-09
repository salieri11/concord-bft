/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';

@Component({
  selector: 'athena-onboarding',
  templateUrl: './onboarding.component.html',
  styleUrls: ['./onboarding.component.scss']
})
export class OnboardingComponent implements OnInit {

  constructor(private router: Router) { }

  ngOnInit() {
  }

  setupOrg(): void {
    this.router.navigate(['dashboard'], {fragment: 'orgTour'});
  }

  deployBlockchain(): void {
    this.router.navigate(['dashboard'], { fragment: 'deploy' });
  }

}
