/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { ClrFormsNextModule } from '@clr/angular';

import { WalletFormComponent } from './wallet-form.component';
import { MockSharedModule } from '../../shared/shared.module';
import { VmwCopyToClipboardButtonComponent } from '../../shared/components/copy-to-clipboard-button/copy-to-clipboard-button.component';

describe('WalletFormComponent', () => {
  let component: WalletFormComponent;
  let fixture: ComponentFixture<WalletFormComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        MockSharedModule,
        ClrFormsNextModule
      ],
      declarations: [
        WalletFormComponent,
        VmwCopyToClipboardButtonComponent
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(WalletFormComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
