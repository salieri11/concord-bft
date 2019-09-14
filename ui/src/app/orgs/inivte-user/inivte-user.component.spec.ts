/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { MockSharedModule } from '../../shared/shared.module';

import { InivteUserComponent } from './inivte-user.component';

describe('InivteUserComponent', () => {
  let component: InivteUserComponent;
  let fixture: ComponentFixture<InivteUserComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [ MockSharedModule ],
      declarations: [ InivteUserComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(InivteUserComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
