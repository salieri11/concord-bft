/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { ReactiveFormsModule } from '@angular/forms';
import { MockTranslateModule } from '../../mocks/mock-translate.module';

import { NodesStatusFilterComponent } from './nodes-status-filter.component';

describe('NodesStatusFilterComponent', () => {
  let component: NodesStatusFilterComponent;
  let fixture: ComponentFixture<NodesStatusFilterComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        ReactiveFormsModule,
        MockTranslateModule
      ],
      declarations: [ NodesStatusFilterComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(NodesStatusFilterComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
