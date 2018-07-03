/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { TranslateLoader, TranslateModule } from "@ngx-translate/core";
import { HttpClient } from "@angular/common/http";
import { HttpClientTestingModule } from "@angular/common/http/testing";

import { ConsortiumsListComponent } from './consortiums-list.component';
import { GridModule } from "../../grid/grid.module";
import { HttpLoaderFactory } from "../../kubernetes/kubernetes.component.spec";
import { ConsortiumService } from "../shared/consortium.service";

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
      declarations: [ ConsortiumsListComponent ],
      providers: [ ConsortiumService ]
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
