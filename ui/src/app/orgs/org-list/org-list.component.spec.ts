/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';

import { GridModule } from '../../grid/grid.module';
import { OrgListComponent } from './org-list.component';
import { OrgService } from '../shared/org.service';
import { MockSharedModule } from '../../shared/shared.module';
import { ActivatedRoute, RouterModule } from '@angular/router';
import { FormsModule } from '@angular/forms';

import { BlockchainService } from '../../shared/blockchain.service';

describe('OrgListComponent', () => {
  let component: OrgListComponent;
  let fixture: ComponentFixture<OrgListComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        MockSharedModule,
        HttpClientTestingModule,
        GridModule
      ],
      declarations: [OrgListComponent],
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
        },
        {
          provide: BlockchainService,
          useValue: {
            selectedBlockchain: {
              consortium_id: 1
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
    fixture = TestBed.createComponent(OrgListComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
