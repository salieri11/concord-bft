/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { TourService as NgxTourService } from 'ngx-tour-ngx-popper';
import { HttpClientTestingModule } from '@angular/common/http/testing';

import { ErrorAlertService } from '../../shared/global-error-handler.service';
import { MockSharedModule } from '../../shared/shared.module';
import { AuthenticationService } from '../../shared/authentication.service';
import { MainComponent } from './main.component';
import { CanViewDirective } from '../../shared/directives/can-view.directive';
import { VmwTaskPanelComponent } from '../../shared/components/task-panel/task-panel.component';
import { VmwTaskComponent } from '../../shared/components/task-panel/task.component';
import { VmwTasksService } from '../../shared/components/task-panel/tasks.service';
import { TourService } from '../../shared/tour.service';

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
        VmwTaskPanelComponent,
        CanViewDirective
      ],
      providers: [
        ErrorAlertService,
        VmwTasksService,
        TourService,
        NgxTourService,

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
      const compiled = testFixture.debugElement.nativeElement;
      expect(compiled.querySelector('.branding .title').textContent).toContain('title');
    }));
  });
});
