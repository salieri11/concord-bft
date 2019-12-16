/*
 * Copyright 2018-2020 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { DeployClientComponent } from './deploy-client.component';
import { MockSharedModule } from '../../shared/shared.module';

describe('DeployClientComponent', () => {
  let component: DeployClientComponent;
  let fixture: ComponentFixture<DeployClientComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [MockSharedModule],
      declarations: [ DeployClientComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(DeployClientComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
