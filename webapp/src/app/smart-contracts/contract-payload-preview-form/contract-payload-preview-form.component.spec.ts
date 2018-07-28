/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { ClarityModule } from '@clr/angular';
import { MockSharedModule } from '../../shared/shared.module';
import { ContractPayloadPreviewFormComponent } from './contract-payload-preview-form.component';


describe('ContractPayloadPreviewFormComponent', () => {
  let component: ContractPayloadPreviewFormComponent;
  let fixture: ComponentFixture<ContractPayloadPreviewFormComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        ClarityModule,
        MockSharedModule
      ],
      declarations: [ ContractPayloadPreviewFormComponent ]
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
});
