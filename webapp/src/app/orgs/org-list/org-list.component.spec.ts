/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { HttpClient } from '@angular/common/http';
import { TranslateLoader, TranslateModule } from '@ngx-translate/core';
import { TranslateHttpLoader } from '@ngx-translate/http-loader';
import { HttpClientTestingModule } from "@angular/common/http/testing";

import { GridModule } from '../../grid/grid.module';
import { OrgListComponent } from './org-list.component';
import { OrgService } from '../shared/org.service';
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
        OrgsService,
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
