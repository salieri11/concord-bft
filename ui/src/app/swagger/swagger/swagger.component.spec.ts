/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { TourService as NgxTourService } from 'ngx-tour-ngx-popper';

import { SwaggerComponent } from './swagger.component';
import { AppHeaderComponent } from '../../shared/components/app-header/app-header.component';
import { MockSharedModule } from '../../shared/shared.module';
import { CanViewDirective } from '../../shared/directives/can-view.directive';
import { VmwThemeSwitchButtonComponent } from '../../shared/components/theme-switch-button/theme-switch-button.component';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { TourService } from '../../shared/tour.service';
import { VmwClarityThemeService } from '../../shared/theme.provider';
import { VersionComponent } from '../../shared/components/version/version.component';

describe('SwaggerComponent', () => {
  let component: SwaggerComponent;
  let fixture: ComponentFixture<SwaggerComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [ RouterTestingModule, MockSharedModule, HttpClientTestingModule ],
      declarations: [
        AppHeaderComponent,
         CanViewDirective,
         SwaggerComponent,
         VmwThemeSwitchButtonComponent,
         VersionComponent
      ],
      providers: [ TourService, NgxTourService, VmwClarityThemeService ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(SwaggerComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
