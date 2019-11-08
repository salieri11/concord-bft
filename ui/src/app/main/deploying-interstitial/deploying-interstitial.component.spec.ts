/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { getSpecTestingModule } from '../../shared/shared.module';
import { DeployingInterstitialComponent } from './deploying-interstitial.component';

describe('DeployingInterstialComponent', () => {
  let component: DeployingInterstitialComponent;
  let fixture: ComponentFixture<DeployingInterstitialComponent>;

  beforeEach(async( async () => {
    const tester = await getSpecTestingModule();
    tester.importLanguagePack();
    TestBed.configureTestingModule(tester.init({
      imports: [], provides: [], declarations: []
    })).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(DeployingInterstitialComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
