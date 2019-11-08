/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { getSpecTestingModule } from '../../shared/shared.module';

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { DeployComponent } from './deploy.component';

describe('DeployComponent', () => {
  let component: DeployComponent;
  let fixture: ComponentFixture<DeployComponent>;

  beforeEach(async( async () => {
    const tester = await getSpecTestingModule();
    tester.importLanguagePack();
    TestBed.configureTestingModule(tester.init({
      imports: [], provides: [], declarations: []
    })).compileComponents();
  }));

  beforeEach( async () => {
    fixture = TestBed.createComponent(DeployComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
