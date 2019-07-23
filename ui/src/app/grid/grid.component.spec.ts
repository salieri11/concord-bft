/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { FormsModule } from '@angular/forms';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { Observable } from 'rxjs';

import { MockSharedModule } from '../shared/shared.module';
import { GridComponent } from './grid.component';
import { ErrorAlertService } from '../shared/global-error-handler.service';


describe('GridComponent', () => {
  let component: GridComponent;
  let fixture: ComponentFixture<GridComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        MockSharedModule,
        RouterTestingModule,
        FormsModule,
        HttpClientTestingModule
      ],
      declarations: [GridComponent],
      providers: [ErrorAlertService]

    })
      .compileComponents();

  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(GridComponent);
    component = fixture.componentInstance;
    component.options = {
      columns: [],
      getData: () => {
        return new Observable(observer => {
          observer.next({
            meta: {
              total: 10,
              size: 50,
              totalPages: 5
            },
            objects: []
          });
          observer.complete();
        });
      },
      paginationTitle: 'string'
    };
      fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
