/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { ActivatedRoute, RouterModule } from '@angular/router';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { BrowserModule } from '@angular/platform-browser';
import { FormsModule } from '@angular/forms';  // <-- #1 import module
import { HttpClientTestingModule } from '@angular/common/http/testing';

import { GridModule } from '../../grid/grid.module';
import { OrgListComponent } from './org-list.component';
import { OrgManagementService } from '../shared/org-management.service';
import { MockSharedModule } from '../../shared/shared.module';

describe('OrgListComponent', () => {
  let component: OrgListComponent;
  let fixture: ComponentFixture<OrgListComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        MockSharedModule,
        BrowserAnimationsModule,
        BrowserModule,
        HttpClientTestingModule,
        FormsModule,
        GridModule
      ],
      declarations: [OrgListComponent],
      providers: [
        OrgManagementService,
        {
          provide: ActivatedRoute,
          useValue: {
            fragment: {
              subscribe: (fn: (value) => void) => fn(
                'add'
              ),
            },
          },
        }]
    })
    .overrideModule(GridModule, {set: {
      imports: [
        FormsModule,
        MockSharedModule,
        RouterModule
      ],
    }})
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(OrgListComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
