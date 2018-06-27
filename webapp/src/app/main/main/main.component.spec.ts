/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';


import { RouterTestingModule } from '@angular/router/testing';
import { ErrorAlertService } from '../../shared/global-error-handler.service';
import { MockSharedModule } from '../../shared/shared.module';
import { AuthenticationService } from '../../shared/authentication.service';

import { MainComponent } from './main.component';

describe('MainComponent', () => {
  let component: MainComponent;
  let fixture: ComponentFixture<MainComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        MockSharedModule
      ],
      declarations: [
        MainComponent,
      ],
      providers: [
        ErrorAlertService
      ]
    }).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(MainComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('when authenticated', () => {
    beforeEach(() => {
      (TestBed.get(AuthenticationService) as AuthenticationService).logIn('test@vmware.com', 'asdfasdf');
    });
    afterEach(() => {
      (TestBed.get(AuthenticationService) as AuthenticationService).logOut();
    });

    it('should render a nav bar title', async(() => {
      const testFixture = TestBed.createComponent(MainComponent);
      testFixture.detectChanges();
      const compiled = fixture.debugElement.nativeElement;
      expect(compiled.querySelector('.branding .title').textContent).toContain('title');
    }));
  });
});
