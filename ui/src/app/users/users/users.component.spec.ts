/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpClient } from '@angular/common/http';
import { ActivatedRoute } from '@angular/router';
import { TranslateLoader, TranslateModule, TranslateService } from '@ngx-translate/core';
import { TranslateHttpLoader } from '@ngx-translate/http-loader';
import { TourService as NgxTourService } from 'ngx-tour-ngx-popper';
import { of as observableOf } from 'rxjs';

import { UsersComponent } from './users.component';
import { UserListComponent } from '../user-list/user-list.component';
import { UserFormComponent } from '../user-form/user-form.component';
import { MockSharedModule } from '../../shared/shared.module';
import { GridModule } from '../../grid/grid.module';
import { UsersService } from '../shared/users.service';
import { TourService } from '../../shared/tour.service';
import { User } from '../shared/user.model';
import { Personas } from '../../shared/persona.service';

export function HttpLoaderFactory(http: HttpClient) {
  return new TranslateHttpLoader(http, './static/i18n/', '.json');
}

const listResponse: User[] = [
  {
    role: Personas.SystemsAdmin,
    user_id: '111-111-111',
    last_login: 1534534154662,
    consortium:
      {
        consortium_id: 2,
        consortium_name: 'TEST_CON'
      },
    organization:
      {
        organization_id: 1,
        organization_name: 'TEST_ORG'
      },
    name: 'Test User',
    details:
      {
        last_name: 'User',
        first_name: 'Test'
      },
    email: 'test.user@vmware.com'
  },
  {
    role: Personas.SystemsAdmin,
    user_id: '222-222-222',
    last_login: 1534534154662,
    consortium:
      {
        consortium_id: 2,
        consortium_name: 'TEST_CON'
      },
    organization:
      {
        organization_id: 1,
        organization_name: 'TEST_ORG'
      },
    name: 'Test User',
    details:
      {
        last_name: 'User',
        first_name: 'Test'
      },
    email: 'test.user.2@vmware.com'
  }
];

describe('UsersComponent', () => {
  let component: UsersComponent;
  let fixture: ComponentFixture<UsersComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        MockSharedModule,
        RouterTestingModule,
        GridModule,
        HttpClientTestingModule,
        BrowserAnimationsModule,
        TranslateModule.forRoot({
          loader: {
            provide: TranslateLoader,
            useFactory: HttpLoaderFactory,
            deps: [HttpClient]
          }
        })
      ],
      declarations: [UsersComponent, UserListComponent, UserFormComponent],
      providers: [
        UsersService,
        TourService,
        NgxTourService,
        TranslateService,
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
      .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(UsersComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('On init', () => {
    it('should load users', () => {
      const getUserSpy = spyOn((component as any), 'getUsers');
      component.ngOnInit();
      expect(getUserSpy).toHaveBeenCalled();
    });
  });

  describe('On fetching users', () => {
    let getListSpy;
    beforeEach(() => {
      getListSpy = spyOn((component as any).usersService, 'getList')
        .and.returnValue(observableOf(listResponse));
    });
    it('should call the getList function from the users service', () => {
      (component as any).getUsers();
      expect(getListSpy).toHaveBeenCalled();
    });

    it('should set the response to the users property', () => {
      (component as any).getUsers();
      expect(component.users).toEqual(listResponse);
    });
  });

  describe('On row selection', () => {
    it('should set the selected property to the rows emitted', () => {
      component.onSelectedUsersChange(listResponse);
      expect(component.selected).toEqual(listResponse);
    });
  });

  describe('After actions', () => {
    let getUserSpy;
    beforeEach(() => {
      getUserSpy = spyOn((component as any), 'getUsers');
    });
    it('should reload users after user add', () => {
      component.addUser();
      expect(getUserSpy).toHaveBeenCalled();
    });

    it('should reload users after user edit', () => {
      component.editUser();
      expect(getUserSpy).toHaveBeenCalled();
    });

    it('should reload users after user delete', () => {
      component.deleteUsers();
      expect(getUserSpy).toHaveBeenCalled();
    });
  });
});
