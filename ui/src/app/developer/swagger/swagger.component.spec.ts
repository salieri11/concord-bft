/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, TestBed, ComponentFixture } from '@angular/core/testing';

import { getSpecTestingModule } from '../../shared/shared-testing.module';
import { SwaggerComponent } from './swagger.component';


describe('SwaggerComponent', () => {
  let component: SwaggerComponent;
  let fixture: ComponentFixture<SwaggerComponent>;

  beforeEach(async( () => {
    const tester = getSpecTestingModule();
    TestBed.configureTestingModule(tester.init({
      imports: [], provides: [], declarations: []
    })).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(SwaggerComponent);
    component = fixture.componentInstance;
    component.ngAfterViewInit(false); // disable
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
