import { Injectable, ErrorHandler } from '@angular/core';
import {HttpErrorResponse} from '@angular/common/http';
import { BehaviorSubject } from 'rxjs/BehaviorSubject';


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
	) {}

	handleError(error: any) {
    // if (error instanceof HttpErrorResponse) {
	    this.alert.add(error);
    // }
  }
}
