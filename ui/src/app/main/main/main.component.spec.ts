/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { TourService as NgxTourService } from 'ngx-tour-ngx-popper';
import { HttpClientTestingModule } from '@angular/common/http/testing';

import { ErrorAlertService } from '../../shared/global-error-handler.service';
import { MockSharedModule } from '../../shared/shared.module';
import { AuthenticationService } from '../../shared/authentication.service';
import { MainComponent } from './main.component';
import { AppHeaderComponent } from '../../shared/components/app-header/app-header.component';
import { CanViewDirective } from '../../shared/directives/can-view.directive';
import { VmwTaskComponent } from '../../shared/components/task-panel/task.component';
import { VmwTasksService } from '../../shared/components/task-panel/tasks.service';
import { TourService } from '../../shared/tour.service';
import { Personas } from '../../shared/persona.service';
import { VmwThemeSwitchButtonComponent } from '../../shared/components/theme-switch-button/theme-switch-button.component';
import { VmwClarityThemeService } from '../../shared/theme.provider';
import { VersionComponent } from '../../shared/components/version/version.component';

describe('MainComponent', () => {
  let component: MainComponent;
  let fixture: ComponentFixture<MainComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        MockSharedModule,
        HttpClientTestingModule
      ],
      declarations: [
        MainComponent,
        VmwTaskComponent,
        CanViewDirective,
        AppHeaderComponent,
        VmwThemeSwitchButtonComponent,
        VersionComponent
      ],
      providers: [
        ErrorAlertService,
        VmwTasksService,
        TourService,
        NgxTourService,
        VmwClarityThemeService

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
      (TestBed.get(AuthenticationService) as AuthenticationService).logIn('test@vmware.com', 'asdfasdf', Personas.SystemsAdmin);
    });
    afterEach(() => {
      (TestBed.get(AuthenticationService) as AuthenticationService).logOut();
    });

    it('should render a nav bar title', async(() => {
      const testFixture = TestBed.createComponent(MainComponent);
      testFixture.detectChanges();
      const compiled = testFixture.debugElement.nativeElement;
      expect(compiled.querySelector('.branding .title').textContent).toContain('title');
    }));
  });
});
