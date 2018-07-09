/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { KubernetesListComponent } from './kubernetes-list.component';
import { GridModule } from '../../grid/grid.module';
import { KubernetesService } from '../shared/kubernetes.service';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { TranslateLoader, TranslateModule } from '@ngx-translate/core';
import { HttpClient } from '@angular/common/http';
import { HttpLoaderFactory } from '../../app.module';
import { FormsModule } from '@angular/forms';
import { RouterModule } from '@angular/router';
import { MockSharedModule } from '../../shared/shared.module';

describe('KubernetesListComponent', () => {
  let component: KubernetesListComponent;
  let fixture: ComponentFixture<KubernetesListComponent>;

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
      declarations: [ KubernetesListComponent ],
      providers: [ KubernetesService ]
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
    fixture = TestBed.createComponent(KubernetesListComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
