/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { ClarityModule } from '@clr/angular';
import { MockSharedModule } from '../../shared/shared.module';

import { ContractPayloadPreviewModalComponent } from './contract-payload-preview-modal.component';

describe('ContractPayloadPreviewModalComponent', () => {
  let component: ContractPayloadPreviewModalComponent;
  let fixture: ComponentFixture<ContractPayloadPreviewModalComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        ClarityModule,
        MockSharedModule
      ],
      declarations: [ ContractPayloadPreviewModalComponent ]
    })
      .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ContractPayloadPreviewModalComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
