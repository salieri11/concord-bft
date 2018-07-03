/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';

import { ConsortiumsListComponent } from './consortiums-list.component';
import { GridModule } from '../../grid/grid.module';
import { ConsortiumService } from '../shared/consortium.service';
import { TranslateLoader, TranslateModule } from '@ngx-translate/core';
import { HttpClient } from '@angular/common/http';
import { HttpLoaderFactory } from '../../app.module';
import { FormsModule } from '@angular/forms';
import { RouterModule } from '@angular/router';
import { MockSharedModule } from '../../shared/shared.module';

describe('ConsortiumsListComponent', () => {
  let component: ConsortiumsListComponent;
  let fixture: ComponentFixture<ConsortiumsListComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        GridModule,
        HttpClientTestingModule,
        TranslateModule.forRoot({
          loader: {
            provide: TranslateLoader,
            useFactory: HttpLoaderFactory,
            deps: [HttpClient]
          }
        })
      ],
      declarations: [ConsortiumsListComponent],
      providers: [ConsortiumService]
    })
      .overrideModule(GridModule, {
        set: {
          imports: [
            FormsModule,
            MockSharedModule,
            RouterModule
          ],
        }
      })
      .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ConsortiumsListComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
