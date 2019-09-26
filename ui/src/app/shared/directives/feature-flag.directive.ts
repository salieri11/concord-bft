/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { Directive, OnInit, Input, ViewContainerRef, TemplateRef } from '@angular/core';
import { FeatureFlagService } from '../feature-flag.service';

@Directive({
  selector: '[concordFeatureFlag]'
})
export class FeatureFlagDirective implements OnInit {
  @Input('concordFeatureFlag') concordFeatureFlag: string | string[];

  constructor(
    private viewContainerRef: ViewContainerRef,
    private templateRef: TemplateRef<any>,
    private featureFlagService: FeatureFlagService
  ) {
  }

  ngOnInit() {
    this.checkFlagsCondition();
  }

  private checkFlagsCondition() {
    if (this.featureFlagService.checkFlagsCondition(this.concordFeatureFlag, this.templateRef)) {
      this.viewContainerRef.createEmbeddedView(this.templateRef);
    } else {
      this.viewContainerRef.clear();
    }
  }

}
