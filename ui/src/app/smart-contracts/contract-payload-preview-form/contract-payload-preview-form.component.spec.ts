/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { ClarityModule } from '@clr/angular';
import { MockSharedModule } from '../../shared/shared.module';
import { ContractPayloadPreviewFormComponent } from './contract-payload-preview-form.component';
import { CodeHighlighterComponent } from '../../shared/components/code-highlighter/code-highlighter.component';


describe('ContractPayloadPreviewFormComponent', () => {
  let component: ContractPayloadPreviewFormComponent;
  let fixture: ComponentFixture<ContractPayloadPreviewFormComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        ClarityModule,
        MockSharedModule
      ],
      declarations: [ContractPayloadPreviewFormComponent, CodeHighlighterComponent]
    })
      .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ContractPayloadPreviewFormComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('On payload preview', () => {
    it('should set isOpen to true', () => {
      const payloadPreview = 'payload string';
      expect(component.isOpen).toBeFalsy();
      component.open(payloadPreview);
      expect(component.isOpen).toBeTruthy();
    });

    it('should set payloadPreview with the passed payload', () => {
      const payloadPreview = 'payload string';
      component.open(payloadPreview);
      expect(component.payloadPreview).toEqual(payloadPreview);
    });
  });

  describe('On closing the preview', () => {
    it('should set isOpen to false', () => {
      const payloadPreview = 'payload string';
      component.open(payloadPreview);
      component.onClose();
      expect(component.isOpen).toBeFalsy();
    });
  });

});
