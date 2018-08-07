/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { of as observableOf, throwError } from 'rxjs';
import { HttpClientTestingModule } from '@angular/common/http/testing';

import { MockSharedModule } from '../../shared/shared.module';
import { TestingGroundComponent } from './testing-ground.component';

const formFilledState = {
  from: '0x10',
  to: '0x10',
  text: '0x10',
  value: '0x10'
};

const expectedApiParams = {
  from: '0x10',
  to: '0x10',
  data: '0x10',
  value: '0x10'
};

const expectedNullCallParams = {
  from: null,
  to: '0x10',
  data: null,
  value: null
};

const expectedNullTransactionParams = {
  from: '0x10',
  to: '0x10',
  data: null,
  value: null
};

describe('TestingGroundComponent', () => {
  let component: TestingGroundComponent;
  let fixture: ComponentFixture<TestingGroundComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        HttpClientTestingModule,
        MockSharedModule
      ],
      declarations: [ TestingGroundComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TestingGroundComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('On call type changes', () => {
    it('does not require the from field when the transaction type is call', () => {
      expect(component.dataForm.controls['from'].errors['required']).toBe(true);
      component.dataForm.get('type').setValue(component.transactionActionOptions.call);
      expect(component.dataForm.controls['from'].errors).toBeFalsy();
    });

    it('requires the from field when the transaction type is transaction', () => {
      component.dataForm.get('type').setValue(component.transactionActionOptions.call);
      expect(component.dataForm.controls['from'].errors).toBeFalsy();
      component.dataForm.get('type').setValue(component.transactionActionOptions.transaction);
      expect(component.dataForm.controls['from'].errors['required']).toBe(true);
    });
  });

  describe('On form input changes', () => {
    it('does not require the value field when the text field has a value', () => {
      expect(component.dataForm.controls['value'].errors['required']).toBe(true);
      component.dataForm.get('text').setValue('testValue');
      expect(component.dataForm.controls['value'].errors).toBeFalsy();
    });

    it('does not require the text field when the value field has a value', () => {
      expect(component.dataForm.controls['text'].errors['required']).toBe(true);
      component.dataForm.get('value').setValue('testValue');
      expect(component.dataForm.controls['text'].errors).toBeFalsy();
    });
  });

  describe('On form submit', () => {
    it('calls submitTransaction if the type is transaction', () => {
      spyOn(component, 'submitTransaction');
      expect(component.dataForm.value.type).toEqual(component.transactionActionOptions.transaction);
      component.onSubmitData();
      expect(component.submitTransaction).toHaveBeenCalled();
    });

    it('calls submitCall if the type is call', () => {
      spyOn(component, 'submitCall');
      component.dataForm.get('type').setValue(component.transactionActionOptions.call);
      expect(component.dataForm.value.type).toEqual(component.transactionActionOptions.call);
      component.onSubmitData();
      expect(component.submitCall).toHaveBeenCalled();
    });

    it('sets the dataHash property on a successful call send', () => {
      component.dataForm.patchValue({type: component.transactionActionOptions.call, ...formFilledState});
      const callSpy = spyOn((component as any).ethApiService, 'sendCall').and.returnValue(observableOf({result: 'result'}));

      component.onSubmitData();

      expect(callSpy).toHaveBeenCalledWith(expectedApiParams);
      expect(component.dataHash).toEqual('result');
    });

    it('sets the dataHash property on a successful transaction send', () => {
      component.dataForm.patchValue(formFilledState);
      const callSpy = spyOn((component as any).ethApiService, 'sendTransaction').and.returnValue(observableOf({result: 'result'}));

      component.onSubmitData();

      expect(callSpy).toHaveBeenCalledWith(expectedApiParams);
      expect(component.dataHash).toEqual('result');
    });

    it('shows an alert on an unsuccessful call send', () => {
      component.dataForm.patchValue({type: component.transactionActionOptions.call, ...formFilledState});
      const alertSpy = spyOn(window, 'alert');
      const callSpy = spyOn((component as any).ethApiService, 'sendCall').and.returnValue(throwError({error: 'error'}));

      component.onSubmitData();

      expect(callSpy).toHaveBeenCalled();
      expect(alertSpy).toHaveBeenCalledWith('error');
    });

    it('shows an alert on an unsuccessful transaction send', () => {
      component.dataForm.patchValue(formFilledState);
      const alertSpy = spyOn(window, 'alert');
      const callSpy = spyOn((component as any).ethApiService, 'sendTransaction').and.returnValue(throwError({error: 'error'}));

      component.onSubmitData();

      expect(callSpy).toHaveBeenCalled();
      expect(alertSpy).toHaveBeenCalledWith('error');
    });

    it('sets from, data, and value properties to null before sending a call if they are empty in the form', () => {
      component.dataForm.patchValue({to: '0x10', type: component.transactionActionOptions.call});
      const callSpy = spyOn((component as any).ethApiService, 'sendCall').and.returnValue(observableOf({result: 'result'}));

      component.onSubmitData();

      expect(callSpy).toHaveBeenCalledWith(expectedNullCallParams);
    });

    it('sets data and value properties to null before sending a transaction if they are empty in the form', () => {
      component.dataForm.patchValue({to: '0x10', from: '0x10'});
      const callSpy = spyOn((component as any).ethApiService, 'sendTransaction').and.returnValue(observableOf({result: 'result'}));

      component.onSubmitData();

      expect(callSpy).toHaveBeenCalledWith(expectedNullTransactionParams);
    });
  });

  describe('On copy to clipboard', () => {
    it('executes the copy command on document', () => {
      const copyFnSpy = spyOn(document, 'execCommand').and.returnValue(true);

      component.onCopyDataHash();

      expect(copyFnSpy).toHaveBeenCalledWith('copy');
    });

    it('shows an alert if the copy command is not supported in the browser', () => {
      spyOn(document, 'execCommand').and.returnValue(false);
      const alertSpy = spyOn(window, 'alert');

      component.onCopyDataHash();

      expect(alertSpy).toHaveBeenCalled();
    });

    it('shows an alert if the copy command throws an error', () => {
      spyOn(document, 'execCommand').and.throwError('error');
      const alertSpy = spyOn(window, 'alert');

      component.onCopyDataHash();

      expect(alertSpy).toHaveBeenCalled();
    });
  });
});
