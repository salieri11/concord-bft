/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { ViewChild, Component } from '@angular/core';
import { ComponentFixture, TestBed, fakeAsync, tick } from '@angular/core/testing';

import { ClarityModule } from '@clr/angular';

import { VmwCopyToClipboardButtonComponent } from './copy-to-clipboard-button.component';

import { customMatchers } from '../../../../custom-matchers';

@Component({
  template: `
        <concord-copy-to-clipboard-button
            [value]="value">
        </concord-copy-to-clipboard-button>
    `
})
class TestHostComponent {
  @ViewChild(VmwCopyToClipboardButtonComponent) component: VmwCopyToClipboardButtonComponent;

  public value = 'Test string';
}

describe('CopyToClipboardButtonComponent', () => {
  let component: VmwCopyToClipboardButtonComponent;
  let hostComponent: TestHostComponent;
  let fixture: ComponentFixture<TestHostComponent>;
  let element: any;

  beforeEach(() => {
    jasmine.addMatchers(customMatchers);
    TestBed.configureTestingModule({
      imports: [
        ClarityModule,
      ],
      declarations: [
        TestHostComponent,
        VmwCopyToClipboardButtonComponent
      ]
    });
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(TestHostComponent);
    element = fixture.debugElement.nativeElement;
    hostComponent = fixture.componentInstance;
    component = hostComponent.component;
  });

  describe('when no btnLabel specified', () => {
    beforeEach(() => {
      fixture.detectChanges();
    });

    it('should initialize size properly', () => {
      component.ngOnInit();
      expect(component.bounds).toBe('18px');
      component.size = 24;
      component.ngOnInit();
      expect(component.bounds).toBe('26px');
    });

    it('should copy the value to the clipboard', () => {
      spyOn(component, 'copyToClipboard');
      component.doCopy();
      expect(component.copyToClipboard).toHaveBeenCalledWith('Test string');
    });

    it('should set the proper classes', fakeAsync(() => {
      expect(component.animClasses).toBe('flip-horizontal-reverse');
      component.doCopy();
      expect(component.animClasses).toBe('flip-horizontal-bottom');
      tick(1500);
      expect(component.animClasses).toBe('flip-horizontal-reverse');
    }));

    it('should show the copy icon', () => {
      expect(element).toHaveRendered('.copy-button');
    });

    it('should not show the copy button with text', () => {
      expect(element).not.toHaveRendered('button.btn.btn-outline');
    });
  });

  describe('when btnLabel specified', () => {
    beforeEach(() => {
      component.btnLabel = 'COPY TO CLIPBOARD';
      fixture.detectChanges();
    });

    it('should show the copy button with text', () => {
      expect(element).toHaveRendered('button.btn.btn-outline');

      const label = element.querySelector('button .face-label span');
      expect(label.innerText).toEqual('COPY TO CLIPBOARD');
    });
  });
});
