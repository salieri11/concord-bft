/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { JoyrideModule } from 'ngx-joyride';

import { UserSettingsComponent } from './user-settings.component';
import { MockSharedModule } from '../../shared/shared.module';
import { CredentialFormComponent } from '../credential-form/credential-form.component';
import { TourService } from '../../shared/tour.service';
import { RouterTestingModule } from '@angular/router/testing';

describe('UserSettingsComponent', () => {
  let component: UserSettingsComponent;
  let fixture: ComponentFixture<UserSettingsComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        MockSharedModule,
        RouterTestingModule,
        JoyrideModule.forRoot()
      ],
      declarations: [ UserSettingsComponent, CredentialFormComponent ],
      providers: [TourService]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(UserSettingsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
