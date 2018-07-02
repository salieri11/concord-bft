/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Directive, OnInit, Input, TemplateRef, ViewContainerRef } from '@angular/core';

import { Personas, PersonaService } from '../persona.service';

@Directive({
  selector: '[athenaCanView]'
})
export class CanViewDirective implements OnInit {
  @Input('athenaCanView') athenaCanView: Personas | Personas[];

  constructor(private templateRef: TemplateRef<any>,
              private viewContainer: ViewContainerRef,
              private personaService: PersonaService) { }

  ngOnInit(): void {
    this.applyPermission();
  }

  private applyPermission(): void {
    if (this.personaService.hasAuthorization(this.athenaCanView) && this.viewContainer.length === 0) {
      this.viewContainer.createEmbeddedView(this.templateRef);
    } else if (!this.personaService.hasAuthorization(this.athenaCanView)) {
      this.viewContainer.clear();
    }
  }
}
