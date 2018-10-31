/*
 * Copyright 2018 VMware, all rights reserved.
 */

import {
  ElementRef,
  Component,
  Input,
  Output,
  EventEmitter,
} from '@angular/core';

import { VmwTaskState } from './tasks.service';

@Component({
  selector: 'athena-task',
  templateUrl: './task.component.html',
  styleUrls: ['./task.component.scss'],
})
export class VmwTaskComponent {
  @Input() progress: number = null;
  @Input() cancelable = false;
  @Input() dismissable = true;
  @Input() state: VmwTaskState;

  @Output() dismissed = new EventEmitter<number>();
  @Output() cancelled = new EventEmitter<number>();
  @Output() cleared = new EventEmitter<number>();

  public hideButtons = false;
  public clickable = false;
  public focused = false;
  public index: number;
  public focus: Function;
  public blur: Function;
  public hidden = false;
  public StateEnum = VmwTaskState;

  constructor(private element: ElementRef) {
    this.element.nativeElement.addEventListener('click', () => {
      this.click();
    });
  }

  dismiss() {
    this.dismissed.emit(this.index);
    this.back();
  }

  clear() {
    this.cleared.emit(this.index);
    this.back();
  }

  cancel() {
    this.cancelled.emit(this.index);
    this.back();
  }

  back() {
    if (this.blur) {
      this.blur();
    }
  }

  click() {
    if (this.focus && this.state === VmwTaskState.IN_PROGRESS) {
      this.focus();
    }
  }
}
