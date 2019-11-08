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
import { WelcomeContentComponent } from './welcome-content.component';
import { CanViewDirective } from '../../shared/directives/can-view.directive';
import { BlockchainService, MockBlockchainsService } from '../../blockchain/shared/blockchain.service';


describe('WelcomeContentomponent', () => {
  let component: WelcomeContentComponent;
  let fixture: ComponentFixture<WelcomeContentComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        MockSharedModule,
        HttpClientTestingModule,
        ReactiveFormsModule
      ],
      declarations: [ WelcomeContentComponent, CanViewDirective ],
      providers: [
        AuthenticationService,
        PersonaService,
        {
          provide: BlockchainService,
          useClass: MockBlockchainsService
        },
      ],
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(WelcomeContentComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
