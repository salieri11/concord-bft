/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ClarityModule } from '@clr/angular';

import { MarketingComponent } from './marketing.component';
import { MockTranslateModule } from '../mocks/mock-translate.module';
import { AuthenticationService } from '../shared/authentication.service';
import { PersonaService } from '../shared/persona.service';

describe('MarketingComponent', () => {
  let component: MarketingComponent;
  let fixture: ComponentFixture<MarketingComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        ClarityModule,
        MockTranslateModule,
        HttpClientTestingModule
      ],
      declarations: [ MarketingComponent ],
      providers: [ AuthenticationService, PersonaService ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(MarketingComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
