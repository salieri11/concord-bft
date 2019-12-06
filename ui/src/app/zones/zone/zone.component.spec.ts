/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { MockSharedModule } from '../../shared/shared.module';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { RouterTestingModule } from '@angular/router/testing';

import { CanViewDirective } from '../../shared/directives/can-view.directive';
import { ConfirmModalComponent } from '../../shared/components/confirm-modal/confirm-modal.component';

import { ZoneComponent } from './zone.component';
import { ZonesService } from '../shared/zones.service';
import { ZoneFormComponent } from '../zone-form/zone-form.component';

import { VmwComboboxComponent } from '../../shared/components/combobox/combobox.component';
import { VmwComboboxItemsComponent } from '../../shared/components/combobox/combobox-items/combobox-items.component';

describe('ZoneComponent', () => {
  let component: ZoneComponent;
  let fixture: ComponentFixture<ZoneComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        MockSharedModule,
        HttpClientTestingModule,
        RouterTestingModule
      ],
      declarations: [
        ZoneComponent,
        ZoneFormComponent,
        CanViewDirective,
        ConfirmModalComponent,
        VmwComboboxComponent,
        VmwComboboxItemsComponent
      ],
      providers: [ZonesService]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ZoneComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
