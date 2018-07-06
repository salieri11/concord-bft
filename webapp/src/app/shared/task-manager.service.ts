/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { BehaviorSubject, Observable } from 'rxjs';
import { first } from 'rxjs/operators';
import { TranslateService } from '@ngx-translate/core';

import { VmwTasksService, VmwTaskState } from './components/task-panel/tasks.service';

@Injectable({
  providedIn: 'root'
})
export class TaskManagerService {
  taskChangeSubject: BehaviorSubject<void>;
  taskChange: Observable<void>;

  private intervals: number[] = [];

  constructor(private tasksService: VmwTasksService, private translateService: TranslateService) {
    this.taskChangeSubject = new BehaviorSubject<void>(null);
    this.taskChange = this.taskChangeSubject.asObservable();

    if (localStorage.getItem('helen.setups.completed') === null) {
      localStorage.setItem('helen.setups.completed', JSON.stringify([]));
    }
    if (localStorage.getItem('helen.setups.pending') === null) {
      localStorage.setItem('helen.setups.pending', JSON.stringify([]));
    }
    if (localStorage.getItem('helen.setups.tasks') === null) {
      localStorage.setItem('helen.setups.tasks', JSON.stringify({}));
    }

    this.buildFromLocalStorage();
  }

  handleBlockchainSetup(blockchainInfo: any) {
    const taskId = randId();

    const pendingSetups = JSON.parse(localStorage.getItem('helen.setups.pending'));
    blockchainInfo.taskId = taskId;
    pendingSetups.push(blockchainInfo);
    localStorage.setItem('helen.setups.pending', JSON.stringify(pendingSetups));

    this.taskChangeSubject.next(null);

    const tasks = [
      {
        title: this.translateService.instant('mockTasks.creatingOrg'),
        progress: -1
      },
      {
        title: this.translateService.instant('mockTasks.addingUsers'),
        progress: -1
      },
      {
        title: this.translateService.instant('mockTasks.creatingConsortium'),
        progress: -1
      },
      {
        title: this.translateService.instant('mockTasks.vmInstance'),
        progress: -1
      },
      {
        title: this.translateService.instant('mockTasks.blockchainInstance'),
        progress: -1
      }
    ];

    const localTasks = JSON.parse(localStorage.getItem('helen.setups.tasks'));

    localTasks[taskId] = tasks;

    localStorage.setItem('helen.setups.tasks', JSON.stringify(localTasks));

    this.addTasks(tasks, taskId);
  }

  resetTasks() {
    const result = this.tasksService.tasks$.pipe(first());

    result.subscribe((tasks) => {
      tasks.forEach((task) => {
        task.clear();
      });
    });

    this.intervals.forEach((interval) => {
      clearInterval(interval);
    });

    localStorage.setItem('helen.setups.completed', JSON.stringify([]));
    localStorage.setItem('helen.setups.pending', JSON.stringify([]));
    localStorage.setItem('helen.setups.tasks', JSON.stringify({}));
  }

  private addTasks(tasks, taskId) {
    const runningTasks = tasks.map((task) => {
      return this.tasksService.trackTask(task);
    });

    this.mockUpdateTasks(runningTasks, taskId);
  }

  private buildFromLocalStorage() {
    const pendingSetups = JSON.parse(localStorage.getItem('helen.setups.pending'));
    if (pendingSetups.length) {
      const localTasks = JSON.parse(localStorage.getItem('helen.setups.tasks'));

      pendingSetups.forEach((setup) => {
        const incompleteTasks = localTasks[setup.taskId].filter(task => task.progress < 100);

        this.addTasks(incompleteTasks, setup.taskId);
      });
    }
  }

  private mockUpdateTasks(tasks, taskId) {
    let currentIndex = 0;

    const interval: number = <any>setInterval(() => {
      const localTasks = JSON.parse(localStorage.getItem('helen.setups.tasks'));

      if (currentIndex === tasks.length) {
        delete localTasks[taskId];
        localStorage.setItem('helen.setups.tasks', JSON.stringify(localTasks));
        clearInterval(interval);
        const pendingSetups = JSON.parse(localStorage.getItem('helen.setups.pending'));
        let completedSetups = JSON.parse(localStorage.getItem('helen.setups.completed'));

        const pendingIndex = pendingSetups.map(x => x.taskId).indexOf(taskId);
        const completedItems = pendingSetups.splice(pendingIndex, 1);

        completedSetups = completedSetups.concat(completedItems);

        localStorage.setItem('helen.setups.pending', JSON.stringify(pendingSetups));
        localStorage.setItem('helen.setups.completed', JSON.stringify(completedSetups));
        this.taskChangeSubject.next(null);
      } else {
        if (tasks[currentIndex].taskInfo.progress < 100) {
          const progress = tasks[currentIndex].taskInfo.progress + 10;
          tasks[currentIndex].taskInfo.progress = progress > 100 ? 100 : progress;
          localTasks[taskId][currentIndex].progress = tasks[currentIndex].taskInfo.progress;
          localStorage.setItem('helen.setups.tasks', JSON.stringify(localTasks));
        }
        if (tasks[currentIndex].taskInfo.progress >= 100) {
          tasks[currentIndex].state = VmwTaskState.COMPLETED;
          currentIndex += 1;
        }
      }
    }, 1000);

    this.intervals.push(interval);
  }
}

function randId() {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
    const r = Math.random() * 16 | 0, v = c === 'x' ? r : (r & 0x3 | 0x8); // tslint:disable-line:no-bitwise
    return v.toString(16);
  });
}
