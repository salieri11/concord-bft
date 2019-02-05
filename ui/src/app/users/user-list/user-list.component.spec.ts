/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, fakeAsync, tick, ComponentFixture, TestBed } from '@angular/core/testing';
import { ActivatedRoute } from '@angular/router';
import { HttpClient } from '@angular/common/http';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { TranslateLoader, TranslateModule, TranslateService } from '@ngx-translate/core';

import { UserListComponent } from './user-list.component';
import { UsersService } from '../shared/users.service';
import { MockSharedModule } from '../../shared/shared.module';
import { GridModule } from '../../grid/grid.module';
import { HttpLoaderFactory } from '../users/users.component.spec';

describe('UserListComponent', () => {
  let component: UserListComponent;
  let fixture: ComponentFixture<UserListComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        MockSharedModule,
        GridModule,
        HttpClientTestingModule,
        TranslateModule.forRoot({
          loader: {
            provide: TranslateLoader,
            useFactory: HttpLoaderFactory,
            deps: [HttpClient]
          }
        })
      ],
      declarations: [ UserListComponent ],
      providers: [ UsersService,
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
    fixture = TestBed.createComponent(UserListComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('On row selection', () => {
    it('should emit the changed rows', fakeAsync(() => {
      const rows = [1, 2, 3];
      const emitSpy = spyOn(component.selected, 'emit');
      (component as any).handleRowSelection(rows);

      tick(20);
      fixture.detectChanges();

      fixture.whenStable().then(() => {
        expect(emitSpy).toHaveBeenCalledWith(rows);
      });
    }));
  });
});
