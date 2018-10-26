/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { MockSharedModule } from '../../shared/shared.module';

import { ConnectWithTruffleComponent } from './connect-with-truffle.component';

describe('ConnectWithTruffleComponent', () => {
  let component: ConnectWithTruffleComponent;
  let fixture: ComponentFixture<ConnectWithTruffleComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        MockSharedModule
      ],
      declarations: [ ConnectWithTruffleComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ConnectWithTruffleComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
