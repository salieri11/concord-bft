/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { HttpClientTestingModule } from "@angular/common/http/testing";
import { RouterTestingModule } from "@angular/router/testing";

import { KubernetesFormComponent } from './kubernetes-form.component';
import { MockSharedModule } from "../../shared/shared.module";
import { KubernetesService } from "../shared/kubernetes.service";

describe('KubernetesFormComponent', () => {
  let component: KubernetesFormComponent;
  let fixture: ComponentFixture<KubernetesFormComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        MockSharedModule,
        HttpClientTestingModule,
        RouterTestingModule
      ],
      declarations: [ KubernetesFormComponent ],
      providers: [ KubernetesService ]
    })
      .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(KubernetesFormComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
