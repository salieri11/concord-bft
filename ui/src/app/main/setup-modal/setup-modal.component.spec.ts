/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ReactiveFormsModule } from '@angular/forms';

import { AuthenticationService } from '../../shared/authentication.service';
import { PersonaService } from '../../shared/persona.service';
import { MockSharedModule } from '../../shared/shared.module';
import { SetupModalComponent } from './setup-modal.component';
import { CanViewDirective } from './../../shared/directives/can-view.directive';
import { BlockchainService, BlockchainsServiceMock } from './../../blockchain/shared/blockchain.service';


describe('SetupModalComponent', () => {
  let component: SetupModalComponent;
  let fixture: ComponentFixture<SetupModalComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        MockSharedModule,
        HttpClientTestingModule,
        ReactiveFormsModule
      ],
      declarations: [ SetupModalComponent, CanViewDirective ],
      providers: [
        AuthenticationService,
        PersonaService,
        {
          provide: BlockchainService,
          useClass: BlockchainsServiceMock
        },
      ],
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(SetupModalComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
