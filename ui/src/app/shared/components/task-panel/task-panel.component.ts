/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import {
  Input,
  ViewEncapsulation,
  Component,
  ViewChildren,
  QueryList,
  AfterViewInit,
  OnInit
} from '@angular/core';
import { VmwTaskComponent } from './task.component';
import {
  style,
  animate,
  transition,
  trigger,
  state,
} from '@angular/animations';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import {
  VmwTasksService,
  VmwTask,
  VmwTaskState,
  Toast
} from './tasks.service';

@Component({
  selector: 'concord-task-panel',
  templateUrl: './task-panel.component.html',
  styleUrls: ['./task-panel.component.scss'],
  encapsulation: ViewEncapsulation.None,
  animations: [
    trigger(
      'collapse', [
        state('collapsed', style({
          height: 0,
          overflow: 'hidden',
          opacity: 0
        })),

        state('open', style({
          height: '*',
          overflow: 'hidden',
          opacity: 1.0
        })),

        transition('open => collapsed', [
          animate('0.2s ease-in-out'),
        ]),

        transition('collapsed => open', [
          animate('0.2s ease-in-out'),
        ]),
      ]
    )
  ]
})
export class VmwTaskPanelComponent implements AfterViewInit, OnInit {

  @ViewChildren(VmwTaskComponent) tasksFromService: QueryList<VmwTaskComponent>;

  /**
   * Enable / Disable hidding all task but the one focused
   */
  @Input()
  hideOthersOnFocus = false;

  /**
   * Enable / Disable dismiss feature:
   * Enable: will show dismiss button and filter out dismissed tasks.
   * Disable: will hide dismiss button and won't filter the dismissed tasks.
   */
  @Input()
  dismissable = true;

  viewModel$: Observable<{
    tasks: VmwTask[];
    runningTasks: number;
  }>;

  state = 'open';
  toasts: Toast[] = [];

  constructor(private vmwTasksService: VmwTasksService) { }

  ngOnInit() {
    this.viewModel$ = this.vmwTasksService.tasks$.pipe(
      map(tasks => {
        const _tasks = this.dismissable ? tasks.filter(t => !t.dismissed) : tasks;

        return {
          tasks: _tasks,
          runningTasks: this.getCountOfRunningTasks(_tasks)
        };
      })
    );
  }

  ngAfterViewInit() {
    this.configureTasks(this.tasksFromService);

    this.tasksFromService.changes.subscribe(() => {
      this.configureTasks(this.tasksFromService);
    });

    this.vmwTasksService.toasts$.subscribe(toast => {
      if (toast) {
        this.toasts.push(toast);
      }
    });

  }

  configureTasks(tasks: QueryList<VmwTaskComponent>) {
    const numTasks = tasks.length;

    // TODO: refactore to avoid using timeouts.
    tasks.forEach((task: VmwTaskComponent, index: number) => {
      setTimeout(() => {
        task.index = index;
        task.hideButtons = numTasks > 1;
      });

      task.focus = () => {
        task.hideButtons = false;
        task.focused = true;

        tasks.forEach((t: VmwTaskComponent) => {
          if (this.hideOthersOnFocus) {
            t.hidden = t !== task;
          }
          t.clickable = false;
        });
      };

      task.blur = () => {
        tasks.forEach((t: VmwTaskComponent) => {
          setTimeout(() => {
            t.focused = false;
            t.hidden = false;
            t.clickable = true;
            t.hideButtons = numTasks > 0;
          });
        });
      };
    });
  }

  toggleState() {
    this.state = this.state === 'open' ? 'collapsed' : 'open';
  }

  panelClasses(runningTasks: number) {
    const classes: any = {};

    if (this.state === 'collapsed') {
      classes['collapsed'] = true;
    }

    if (runningTasks > 0) {
      classes['running'] = true;
    }

    return classes;
  }

  taskClasses(task: VmwTask) {
    const classes: any = {};

    if (task.state === VmwTaskState.COMPLETED) {
      classes['completed'] = true;
    }

    if (task.state === VmwTaskState.ERROR) {
      classes['failed'] = true;
    }

    if (task.state === VmwTaskState.CANCELLED) {
      classes['cancelled'] = true;
    }

    classes['clickable'] = task.state === VmwTaskState.IN_PROGRESS;

    return classes;
  }

  cancelTask(task: VmwTask) {
    task.cancel();
  }

  dismissTask(task: VmwTask) {
    task.dismissed = true;
  }

  clearTask(task: VmwTask) {
    task.clear();
  }

  private getCountOfRunningTasks(tasks: VmwTask[]) {
    return tasks.filter(t => t.state === VmwTaskState.IN_PROGRESS).length;
  }
}
