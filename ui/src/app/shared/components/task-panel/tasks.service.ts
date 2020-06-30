/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { BehaviorSubject,  Observable,  Subject,  PartialObserver } from 'rxjs';
import { mapTo, takeUntil, tap } from 'rxjs/operators';
import { VmwToastType } from '@vmw/ngx-components';

export interface IVmwTaskInfo {
  /**
   * Title of the task
   */
  title: string;

  /**
   * Description of the task
   */
  description?: string;

  /**
   * Failure message
   */
  failedMessage?: string;

  /**
   * Completion message
   */
  completedMessage?: string;

  /**
   * The progress of the Task in a range of 0 to 100 (%)
   * If a task doesn't have a progress, set this property to -1 so that the
   * component can show an Indeterminate Progress Bar
   */
  progress: number;

  /**
   * The time remaining for the task to finish in seconds
   */
  remaining?: number;
}

export enum VmwTaskState {
  IN_PROGRESS = <any>'IN_PROGRESS',
  COMPLETED = <any>'COMPLETED',
  ERROR = <any>'ERROR',
  CANCELLED = <any>'CANCELLED'
}

export interface Toast {
  title: string;
  description?: string;
  date?: string;
  type: VmwToastType;
  primaryActionTitle?: String;
  primaryActionHandler?: any;
  alreadyShown?: boolean;
}

export class VmwTask {

  /**
   * A Subject to notifiy when something changed in the task.
   * It will emit the instance of the task to the subscriber
   */
  private _changedSubject: Subject<VmwTask> = new Subject();

  /**
   * Subject that emits when a task is being cancelled
   */
  private _cancellingSubject = new Subject();

  /**
   * A Subject to notify when the task is cleared.
   */
  private _clearSubject = new Subject();

  /**
   * Current State of the task
   * @default IN_PROGRESS
   */
  private _state = VmwTaskState.IN_PROGRESS;

  /**
   * Dismissed state of the task
   */
  private _dismissed = false;

  /**
   * An Observable to nofity any changes in the task
   */
  changed$: Observable<VmwTask> = this._changedSubject.asObservable();

  /**
   * An Observable to nofity when the task is cleared
   */
  cleared$ = this._clearSubject.asObservable();

  /**
   * Reference to the Task Info provided in Construction
   */
  taskInfo: IVmwTaskInfo;

  /**
   * If a cancelObserver is provided, this flag will be true
   */
  cancelable = false;

  /**
   * On construction, the class receives this parameters:
   * @param taskInfo a VmwTaskInfo object
   * @param cancelObserver a partial observer to be notified when the task is cancelled
   */
  constructor(taskInfo: IVmwTaskInfo, cancelObserver?: PartialObserver<any>) {
    this.taskInfo = taskInfo;

    if (cancelObserver) {
      this.cancelable = true;
      /**
       * Stream to let the consumer of this class know that this task was Cancelled
       */
      this._cancellingSubject
      // Pass the task instance to the consumer
        .pipe(
          mapTo(this),
          takeUntil(this.cleared$)
        )
        .subscribe(cancelObserver);
    }
  }

  get state(): VmwTaskState {
    return this._state;
  }

  set state(state: VmwTaskState) {
    this._state = state;
    this._changedSubject.next(this);
  }

  get dismissed(): boolean {
    return this._dismissed;
  }

  set dismissed(dismiss: boolean) {
    this._dismissed = dismiss;
    this._changedSubject.next(this);
  }

  clear() {
    this._clearSubject.next();
  }

  /**
   * Cancel a Task
   * The task doesn't change it's state. It's responsibility of the consumer to change
   * the state after the cancel operation is completed. This also gives the change to
   * the consumer to show a confirmation before cancelling.
   */
  cancel() {
    this._cancellingSubject.next();
  }
}

@Injectable()
export class VmwTasksService {

  private _taskListSubject: BehaviorSubject<VmwTask[]> = new BehaviorSubject([]);
  private _toastListSubject: BehaviorSubject<Toast> = new BehaviorSubject(null);

  tasks$: Observable<VmwTask[]> = this._taskListSubject.asObservable();
  toasts$: Observable<Toast> = this._toastListSubject.asObservable();

  /**
   * Includes the given Task in the inner TasksList
   * @param taskInfo the basic Information of the Task
   * @param cancelObserver [optional] an observer to be notified when the task is cancelled
   * @return a VmkTask object
   */
  trackTask(taskInfo: IVmwTaskInfo, cancelObserver?: PartialObserver<any>): VmwTask {
    const task = new VmwTask(taskInfo, cancelObserver);
    this._taskListSubject.next([task, ...this._taskListSubject.value]);
    task.changed$.pipe(
      takeUntil(task.cleared$.pipe(tap(() => this.clearTask(task))))
    ).subscribe(() => {
      this._taskListSubject.next(this._taskListSubject.value);
    });

    return task;
  }

  addToast(toast: Toast) {
    this._toastListSubject.next(toast);
    setTimeout(() => { toast.alreadyShown = true; }, 0);
  }

  // removeToast(index: number) {
  //     this.toasts.splice(index, 1);
  // }

  /**
   * Clear the given task
   * @param task the basic Information of the Task
   */
  private clearTask(task: VmwTask) {
    this._taskListSubject.next(this._taskListSubject.value.filter(t => t !== task));
  }
}
