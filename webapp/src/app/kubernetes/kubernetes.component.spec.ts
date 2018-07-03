/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { BrowserModule } from '@angular/platform-browser';
import { FormsModule } from '@angular/forms';  // <-- #1 import module
import { HttpClientModule } from '@angular/common/http';
import { ActivatedRoute, RouterModule } from '@angular/router';

import { GridModule } from '../grid/grid.module';
import { KubernetesService } from './shared/kubernetes.service';
import { MockSharedModule } from '../shared/shared.module';
import { KubernetesComponent } from './kubernetes.component';
import { KubernetesListComponent } from './kubernetes-list/kubernetes-list.component';
import { KubernetesFormComponent } from './kubernetes-form/kubernetes-form.component';

describe('KubernetesComponent', () => {
  let component: KubernetesComponent;
  let fixture: ComponentFixture<KubernetesComponent>;

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
      declarations: [ KubernetesComponent, KubernetesListComponent, KubernetesFormComponent ],
      providers: [
        KubernetesService,
        {
          provide: ActivatedRoute,
          useValue: {
            fragment: {
              subscribe: (fn: (value) => void) => fn(
                'add'
              ),
            }
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
    fixture = TestBed.createComponent(KubernetesComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
