/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { BrowserModule } from '@angular/platform-browser';
import { FormsModule } from '@angular/forms';  // <-- #1 import module
import { HttpClientModule } from '@angular/common/http';
import { ActivatedRoute, RouterModule } from '@angular/router';

import { GridModule } from '../grid/grid.module';
import { OrgListComponent } from './org-list/org-list.component';
import { OrgService } from './shared/org.service';
import { OrgFormComponent } from './org-form/org-form.component';
import { MockSharedModule } from '../shared/shared.module';
import { OrgsComponent } from './orgs.component';

describe('OrgsComponent', () => {
  let component: OrgsComponent;
  let fixture: ComponentFixture<OrgsComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        MockSharedModule,
        BrowserAnimationsModule,
        BrowserModule,
        HttpClientModule,
        FormsModule,
        GridModule
      ],
      declarations: [
        OrgsComponent,
        OrgListComponent,
        OrgFormComponent
      ],
      providers: [
        OrgService,
        {
          provide: ActivatedRoute,
          useValue: {
            fragment: {
              subscribe: (fn: (value) => void) => fn(
                'add'
              ),
            },
          },
        }
      ]
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
    fixture = TestBed.createComponent(OrgsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
