/*
 * Copyright 2020 VMware, all rights reserved.
 */
/* tslint:disable:no-unused-variable */
import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { WavefrontGraphComponent } from './wavefront-graph.component';
import { getSpecTestingModule } from '../../shared/shared-testing.module';

describe('LineGraphComponent', () => {
  let component: WavefrontGraphComponent;
  let fixture: ComponentFixture<WavefrontGraphComponent>;

  beforeEach(async( () => {
    const tester = getSpecTestingModule();
    TestBed.configureTestingModule(tester.init({
      imports: [], provides: [], declarations: []
    })).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(WavefrontGraphComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
