/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule } from '@angular/forms';  // <-- #1 import module
import { RouterModule } from '@angular/router';
import { NoopAnimationsModule } from '@angular/platform-browser/animations';
import { ActivatedRoute } from '@angular/router';
import { HttpClientTestingModule } from '@angular/common/http/testing';

import { GridModule } from '../grid/grid.module';
import { BlockchainsComponent } from './blockchains.component';
import { MockSharedModule } from '../shared/shared.module';
import { BlockchainsService } from './shared/blockchains.service';
import { KubernetesService } from '../kubernetes/shared/kubernetes.service';
import { OrgService } from '../orgs/shared/org.service';
import { BlockchainsListComponent } from "./blockchains-list/blockchains-list.component";
import { BlockchainFormComponent } from "./blockchain-form/blockchain-form.component";

describe('BlockchainsComponent', () => {
  let component: BlockchainsComponent;
  let fixture: ComponentFixture<BlockchainsComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        HttpClientTestingModule,
        FormsModule,
        GridModule,
        MockSharedModule,
        NoopAnimationsModule
      ],
      declarations: [ BlockchainsComponent, BlockchainsListComponent, BlockchainFormComponent ],
      providers: [
        BlockchainsService,
        OrgService,
        KubernetesService,
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
    fixture = TestBed.createComponent(BlockchainsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
