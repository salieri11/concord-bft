/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { RouterTestingModule } from '@angular/router/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { MockSharedModule } from '../../shared/shared.module';

import { BlockDetailsComponent } from '../block-details/block-details.component';
import { BlocksListComponent } from './blocks-list.component';

describe('BlocksListComponent', () => {
  let component: BlocksListComponent;
  let fixture: ComponentFixture<BlocksListComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        HttpClientTestingModule,
        MockSharedModule
      ],
      declarations: [
        BlocksListComponent,
        BlockDetailsComponent
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(BlocksListComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
