/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable, ErrorHandler } from '@angular/core';
import { BehaviorSubject } from 'rxjs';

@Injectable()
export class ErrorAlertService {
  notify: BehaviorSubject<any> = new BehaviorSubject(null);

  constructor() {
  }

  add(error: Error) {
    this.notify.next(error);
    console.error(error);
  }
}

@Injectable()
export class GlobalErrorHandlerService implements ErrorHandler {

  constructor(
    private alert: ErrorAlertService
  ) { }

  handleError(error: any) {
    this.alert.add(error);
  }
}
