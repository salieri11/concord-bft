/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component } from '@angular/core';
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { TourService as NgxTourService } from 'ngx-tour-ngx-popper';

import { AppHeaderComponent } from './app-header.component';
import { MockSharedModule } from '../../shared.module';
import { CanViewDirective } from '../../directives/can-view.directive';
import { VmwThemeSwitchButtonComponent } from '../theme-switch-button/theme-switch-button.component';
import { TourService } from '../../tour.service';
import { VmwClarityThemeService } from '../../theme.provider';

@Component({
  selector: 'concord-test-wrapper',
  template: `
        <clr-main-container>
            <concord-app-header></concord-app-header>
        </clr-main-container>
  `
})
class TestWrapperClassComponent {}

describe('AppHeaderComponent', () => {
  let component: TestWrapperClassComponent;
  let fixture: ComponentFixture<TestWrapperClassComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [ RouterTestingModule, MockSharedModule, HttpClientTestingModule ],
      declarations: [ AppHeaderComponent, CanViewDirective, VmwThemeSwitchButtonComponent, TestWrapperClassComponent ],
      providers: [ TourService, NgxTourService, VmwClarityThemeService ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TestWrapperClassComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
