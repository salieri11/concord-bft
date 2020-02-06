/*
 * Copyright 2019-2020 VMware, all rights reserved.
 */
import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { MockSharedModule } from '../../shared/shared.module';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { RouterTestingModule } from '@angular/router/testing';

import { CanViewDirective } from '../../shared/directives/can-view.directive';
import { ConsortiumService } from '../shared/consortium.service';

import { ConsortiumListComponent } from './consortium-list.component';

describe('ConsortiumListComponent', () => {
  let component: ConsortiumListComponent;
  let fixture: ComponentFixture<ConsortiumListComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        MockSharedModule,
        HttpClientTestingModule,
        RouterTestingModule
      ],
      declarations: [ ConsortiumListComponent, CanViewDirective ],
      providers: [ConsortiumService]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ConsortiumListComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
