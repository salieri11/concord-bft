/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Directive, OnInit, Input, TemplateRef, ViewContainerRef } from '@angular/core';

import { PersonaService } from '../persona.service';

@Directive({
  selector: '[appCanView]'
})
export class CanViewDirective implements OnInit {
  @Input('appCanView') appCanView: string | string[];

  constructor(private templateRef: TemplateRef<any>,
              private viewContainer: ViewContainerRef,
              private personaService: PersonaService) { }

  ngOnInit(): void {
    this.applyPermission();
  }

  private applyPermission(): void {
    if (this.personaService.hasAuthorization(this.appCanView) && this.viewContainer.length === 0) {
      this.viewContainer.createEmbeddedView(this.templateRef);
    } else if (!this.personaService.hasAuthorization(this.appCanView)) {
      this.viewContainer.clear();
    }
  }
}
