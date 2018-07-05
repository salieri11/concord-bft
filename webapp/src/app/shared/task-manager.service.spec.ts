/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { TestBed, inject } from '@angular/core/testing';

import { TaskManagerService } from './task-manager.service';
import { VmwTasksService } from './components/task-panel/tasks.service';

describe('TaskManagerService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [TaskManagerService, VmwTasksService]
    });
  });

  it('should be created', inject([TaskManagerService], (service: TaskManagerService) => {
    expect(service).toBeTruthy();
  }));
});
