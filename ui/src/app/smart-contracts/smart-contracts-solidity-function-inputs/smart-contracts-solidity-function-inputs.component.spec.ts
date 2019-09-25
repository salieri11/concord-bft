/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, ViewChild } from '@angular/core';
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { FormGroup } from '@angular/forms';

import { SmartContractsSolidityFunctionInputsComponent } from './smart-contracts-solidity-function-inputs.component';
import { MockSharedModule } from '../../shared/shared.module';
import { AbiFunctionParameter } from '../shared/smart-contracts.model';

@Component({
  selector: 'concord-test-wrapper',
  template: `
        <concord-smart-contracts-solidity-function-inputs #solidityInputs
        [formGroup]="functionInputForm"
        [functionInputs]="functionInputs"></concord-smart-contracts-solidity-function-inputs>
    `
})
class TestWrapperClassComponent {
  @ViewChild('solidityInputs', { static: false }) solidityInputs: SmartContractsSolidityFunctionInputsComponent;
  functionInputForm: FormGroup = new FormGroup({});
  functionInputs: AbiFunctionParameter[] = [];
}

describe('SmartContractsSolidityFunctionInputsComponent', () => {
  let component: TestWrapperClassComponent;
  let fixture: ComponentFixture<TestWrapperClassComponent>;
  let inputs: AbiFunctionParameter[];

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        MockSharedModule,
      ],
      declarations: [
        TestWrapperClassComponent,
        SmartContractsSolidityFunctionInputsComponent
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TestWrapperClassComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();

    inputs = [{
      type: 'uint8',
      name: 'testUint'
    }, {
      type: 'int8',
      name: 'testInt'
    }, {
      type: 'address',
      name: 'testAddress'
    }, {
      type: 'other',
      name: 'testOther'
    }];

    expect(component.solidityInputs.formGroup.controls).toEqual({});

    component.functionInputs = inputs;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('On changes', () => {
    it('adds controls to the form group', () => {
      inputs.forEach((input) => {
        expect(component.solidityInputs.formGroup.contains(input.name)).toBe(true);
      });
    });
  });

  describe('Form building', () => {

    it('returns a signed int validated form control for int control types', () => {
      component.solidityInputs.formGroup.controls['testInt'].setValue('0xffff');

      expect(component.solidityInputs.formGroup.controls['testInt'].errors['signedInteger']).toBe(true);
    });

    it('returns a uint validated form control for uint control types', () => {
      component.solidityInputs.formGroup.controls['testUint'].setValue('0xffff');

      expect(component.solidityInputs.formGroup.controls['testUint'].errors['unsignedInteger']).toBe(true);
    });

    it('returns an address validated form control for address control types', () => {
      component.solidityInputs.formGroup.controls['testAddress'].setValue('test');

      expect(component.solidityInputs.formGroup.controls['testAddress'].errors['hexAddress']).toBe(true);
    });

    it('returns a required validated form control for other control control types', () => {
      component.solidityInputs.formGroup.controls['testOther'].setValue('');

      expect(component.solidityInputs.formGroup.controls['testOther'].errors['required']).toBe(true);
    });
  });
});
