/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from "@angular/router/testing";
import { HttpClientTestingModule } from "@angular/common/http/testing";

import { BlockchainFormComponent } from './blockchain-form.component';
import { KubernetesService } from "../../kubernetes/shared/kubernetes.service";
import { BlockchainsService } from "../shared/blockchains.service";
import { OrgService } from "../../orgs/shared/org.service";
import { MockSharedModule } from "../../shared/shared.module";
import { ErrorAlertService } from "../../shared/global-error-handler.service";

describe('BlockchainFormComponent', () => {
  let component: BlockchainFormComponent;
  let fixture: ComponentFixture<BlockchainFormComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        HttpClientTestingModule,
        RouterTestingModule,
        MockSharedModule
      ],
      declarations: [
        BlockchainFormComponent,
      ],
      providers: [
        BlockchainsService,
        KubernetesService,
        OrgService,
        ErrorAlertService
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(BlockchainFormComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
