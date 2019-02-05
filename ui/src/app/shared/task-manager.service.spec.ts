/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { TestBed, inject } from '@angular/core/testing';
import { TranslateService } from '@ngx-translate/core';

import { TaskManagerService } from './task-manager.service';
import { VmwTasksService } from './components/task-panel/tasks.service';
import { MockTranslateService } from '../mocks/mock-translate.module';


describe('TaskManagerService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [
        TaskManagerService,
        VmwTasksService,
        {provide: TranslateService, useClass: MockTranslateService}
      ]
    });
  });

  it('should be created', inject([TaskManagerService], (service: TaskManagerService) => {
    expect(service).toBeTruthy();
  }));
});
