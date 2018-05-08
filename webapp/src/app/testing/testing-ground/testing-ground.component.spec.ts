/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { ClarityModule } from '@clr/angular';
import { MockTranslateModule } from '../../mocks/mock-translate.module';
import { SharedModule } from '../../shared/shared.module';

import { TestingGroundComponent } from './testing-ground.component';

describe('TestingGroundComponent', () => {
  let component: TestingGroundComponent;
  let fixture: ComponentFixture<TestingGroundComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        HttpClientTestingModule,
        ReactiveFormsModule,
        ClarityModule,
        MockTranslateModule,
        SharedModule
      ],
      declarations: [ TestingGroundComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TestingGroundComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
