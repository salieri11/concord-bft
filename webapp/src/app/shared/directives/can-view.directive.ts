/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Directive, OnInit, OnDestroy, Input, TemplateRef, ViewContainerRef } from '@angular/core';
import { Subscription } from 'rxjs/Subscription';

import { PersonaService } from '../persona.service';

@Directive({
  selector: '[appCanView]'
})
export class CanViewDirective implements OnInit, OnDestroy {
  @Input('appCanView') appCanView: string | string[];
  private subscription: Subscription;

  constructor(private templateRef: TemplateRef<any>,
              private viewContainer: ViewContainerRef,
              private personaService: PersonaService) { }

  ngOnInit(): void {
    this.applyPermission();
    this.subscription = this.personaService.personaChanged$.subscribe(() => {
      this.applyPermission();
    });
  }

  private applyPermission(): void {
    if (this.personaService.hasAuthorization(this.appCanView) && this.viewContainer.length === 0) {
      this.viewContainer.createEmbeddedView(this.templateRef);
    } else if (!this.personaService.hasAuthorization(this.appCanView)) {
      this.viewContainer.clear();
    }
  }

  ngOnDestroy() {
    this.subscription.unsubscribe();
  }
}
