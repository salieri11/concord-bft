/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { SmartContractsSolidityFunctionInputsComponent } from './smart-contracts-solidity-function-inputs.component';

describe('SmartContractsSolidityFunctionInputsComponent', () => {
  let component: SmartContractsSolidityFunctionInputsComponent;
  let fixture: ComponentFixture<SmartContractsSolidityFunctionInputsComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ SmartContractsSolidityFunctionInputsComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(SmartContractsSolidityFunctionInputsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
