/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { MockSharedModule } from '../../shared/shared.module';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';

import { ContractFormComponent } from '../contract-form/contract-form.component';
import { SmartContractsComponent } from './smart-contracts.component';
import { CanViewDirective } from '../../shared/directives/can-view.directive';

describe('SmartContractsComponent', () => {
  let component: SmartContractsComponent;
  let fixture: ComponentFixture<SmartContractsComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        HttpClientTestingModule,
        MockSharedModule
      ],
      declarations: [ SmartContractsComponent, ContractFormComponent, CanViewDirective ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(SmartContractsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
