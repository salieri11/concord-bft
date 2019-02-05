/*
 * Copyright 2018-2019 VMware, all rights reserved.
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
      setCompletedSetups([]);
    }
    if (localStorage.getItem('helen.setups.pending') === null) {
      setPendingSetups([]);
    }
    if (localStorage.getItem('helen.setups.tasks') === null) {
      setLocalTasks({});
    }

    this.buildFromLocalStorage();
  }

  handleBlockchainSetup(blockchainInfo: any) {
    const taskId = randId();

    const pendingSetups = getPendingSetups();
    blockchainInfo.taskId = taskId;
    pendingSetups.push(blockchainInfo);
    setPendingSetups(pendingSetups);

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

    const localTasks = getLocalTasks();

    localTasks[taskId] = tasks;

    setLocalTasks(localTasks);

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

    setCompletedSetups([]);
    setPendingSetups([]);
    setLocalTasks({});
    this.taskChangeSubject.next(null);
  }

  private addTasks(tasks, taskId) {
    const runningTasks = tasks.map((task) => {
      return this.tasksService.trackTask(task);
    });

    this.mockUpdateTasks(runningTasks, taskId);
  }

  private buildFromLocalStorage() {
    const pendingSetups = getPendingSetups();
    if (pendingSetups.length) {
      const localTasks = getLocalTasks();

      pendingSetups.forEach((setup) => {
        const incompleteTasks = localTasks[setup.taskId].filter(task => task.progress < 100);

        this.addTasks(incompleteTasks, setup.taskId);
      });
    }
  }

  private mockUpdateTasks(tasks, taskId) {
    let currentIndex = 0;

    const interval: number = <any>setInterval(() => {
      const localTasks = getLocalTasks();

      if (currentIndex === tasks.length) {
        delete localTasks[taskId];
        setLocalTasks(localTasks);
        clearInterval(interval);
        const pendingSetups = getPendingSetups();
        let completedSetups = getCompletedSetups();

        const pendingIndex = pendingSetups.map(x => x.taskId).indexOf(taskId);
        const completedItems = pendingSetups.splice(pendingIndex, 1);

        completedSetups = completedSetups.concat(completedItems);

        setPendingSetups(pendingSetups);
        setCompletedSetups(completedSetups);
        this.taskChangeSubject.next(null);
      } else {
        if (tasks[currentIndex].taskInfo.progress < 100) {
          const progress = tasks[currentIndex].taskInfo.progress + 3;
          tasks[currentIndex].taskInfo.progress = progress > 100 ? 100 : progress;
          localTasks[taskId][currentIndex].progress = tasks[currentIndex].taskInfo.progress;
          setLocalTasks(localTasks);
        }
        if (tasks[currentIndex].taskInfo.progress >= 100) {
          tasks[currentIndex].state = VmwTaskState.COMPLETED;
          currentIndex += 1;
        }
      }
    }, 1000);

    this.intervals.push(interval);
  }

  getMockInitData(): Array<any> {
    return [
      {
        'blockchain': {
          'type': 'hyperledger'
        },
        'faultTolerance': {
          'type': 'local'
        },
        'consortium': {
          'name': 'Consortium 1'
        },
        'organizations': [{
          'name': 'Bank Of China',
          'location': 'beijing'
        }],
        'users': [{
          'firstName': 'John',
          'lastName': 'Doe',
          'email': 'jdoe@bankofchina.com',
          'organization': 'Bank Of China',
          'role': 'system_admin'
        }],
        'advancedSettings': {
          'networkName': 'Consortium 1 Net',
          'numberOfNodes': 36,
          'publicNodesRegions': [{
            'value': 'us-east-1',
            'displayValue': 'AWS US East (N. Virginia)'
          }, {
            'value': 'us-east-2',
            'displayValue': 'AWS US East (Ohio)'
          }, {
            'value': 'us-west-1',
            'displayValue': 'AWS US West (N. California)'
          }, {
            'value': 'us-west-2',
            'displayValue': 'AWS US West (Oregon)'
          }, {
            'value': 'ap-northeast-1',
            'displayValue': 'AWS Asia Pacific (Tokyo)'
          }, {
            'value': 'ap-northeast-3',
            'displayValue': 'AWS Asia Pacific (Osaka-Local)'
          }, {
            'value': 'ap-south-1',
            'displayValue': 'AWS Asia Pacific (Mumbai)'
          }, {
            'value': 'ap-southeast-1',
            'displayValue': 'AWS Asia Pacific (Singapore)'
          }, {
            'value': 'ap-southeast-2',
            'displayValue': 'AWS Asia Pacific (Sydney)'
          }, {
            'value': 'ca-central-1',
            'displayValue': 'AWS Canada (Central)'
          }, {
            'value': 'cn-north-1',
            'displayValue': 'AWS China (Beijing)'
          }, {
            'value': 'cn-northwest-1',
            'displayValue': 'AWS China (Ningxia)'
          }, {
            'value': 'eu-central-1',
            'displayValue': 'AWS EU (Frankfurt)'
          }, {
            'value': 'eu-west-1',
            'displayValue': 'AWS EU (Ireland)'
          }, {
            'value': 'eu-west-2',
            'displayValue': 'AWS EU (London)'
          }, {
            'value': 'eu-west-3',
            'displayValue': 'AWS EU (Paris)'
          }, {
            'value': 'sa-east-1',
            'displayValue': 'AWS South America (São Paulo)'
          }],
          'privateNode': []
        },
        'taskId': '9d0c3c20-3be5-4dc8-a4f5-2880acd73f84'
      }
    ];
  }
}

function randId() {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
    const r = Math.random() * 16 | 0, v = c === 'x' ? r : (r & 0x3 | 0x8); // tslint:disable-line:no-bitwise
    return v.toString(16);
  });
}

function getLocalTasks() {
  return JSON.parse(localStorage.getItem('helen.setups.tasks'));
}

function setLocalTasks(localTasks) {
  localStorage.setItem('helen.setups.tasks', JSON.stringify(localTasks));
}

export function getPendingSetups() {
  return JSON.parse(localStorage.getItem('helen.setups.pending'));
}

function setPendingSetups(pendingSetups) {
  localStorage.setItem('helen.setups.pending', JSON.stringify(pendingSetups));
}

export function getCompletedSetups() {
  return JSON.parse(localStorage.getItem('helen.setups.completed'));
}

export function setCompletedSetups(completedSetups) {
  localStorage.setItem('helen.setups.completed', JSON.stringify(completedSetups));
}
