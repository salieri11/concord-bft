/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable, ErrorHandler } from '@angular/core';
import { BehaviorSubject, Subject } from 'rxjs';
import { environment } from '../../environments/environment';

import {
  CspHeaderAppAlertType,
  CspHeaderAppAlertConfig,
} from '@vmw/csp-ngx-components';

interface CspHeaderAppAlertEntry {
  timeCreated: number;
  alertData: CspHeaderAppAlertConfig;
}

@Injectable({
  providedIn: 'root'
})
export class ErrorAlertService {
  env = environment;
  notify: BehaviorSubject<any> = new BehaviorSubject(null);

  cspAlertById: {[id: string]: CspHeaderAppAlertEntry} = {};
  cspAlertStream: Subject<CspHeaderAppAlertConfig> = new Subject<CspHeaderAppAlertConfig>();

  constructor() {
  }

  /**
   * @param {Error} error
   * Error object to used to make an app-level alert (e.g. new Error('Your error message'))
   *
   * ---
   * @param {CspHeaderAppAlertConfig} alertData
   * Actionable app-level alert data handled by CSP-Header. \
   * {
   * `id`: string; (if null, auto-generated) \
   * `type`?: CspHeaderAppAlertType.(DANGER | WARNING | INFO | SUCCESS); \
   * `content`?: string; (if null, error.message is used) \
   * `actionText`?: string; \
   * `action`?: Function; \
   * `closable`?: boolean; (if null, true)\
   * `onClose`?: Function; \
   * `priority`?: CspHeaderAppAlertPriority.(NORMAL | HIGH | TOP); \
   * }
   */
  add(error: Error, alertData: CspHeaderAppAlertConfig = null): string {

    /* In non-CSP, just use regular notify which uses clarity default alert system
       the default clarity alert system resides in `<clr-alerts>` in `main.component.html`
    */
    if (!this.env.csp) {

      this.notify.next(error);

    } else { // Use CSP-Header provided alert system in CSP mode

      const alertId = (alertData && alertData.id) ? alertData.id : 'cspError_' + Date.now();

      if (!alertData) {
        alertData = {
          id: alertId,
          type: CspHeaderAppAlertType.DANGER,
          content: error.message
        };
      }

      if (alertData.id == null) { alertData.id = alertId; }
      if (alertData.content == null) { alertData.content = error.message; }
      if (alertData.type == null) { alertData.type = CspHeaderAppAlertType.DANGER; }

      // Nobody wants unclosable alerts by default; if not explicitly `false`, do make it closable.
      if (alertData.closable == null) { alertData.closable = true; }

      /* This method is adding alert only. Reject `remove` flag to prevent weird behavior from orphaned alerts. */
      if (alertData.remove === true) {
        console.log(new Error(`ErrorAlertService: cannot simultaneously pass 'remove' flag while adding an alert.`));
        return null;
      }

      /* Oddly, alerts with non-unique ID doesn't get rejected by CSP Header; they just simply won't show,
         so reject it here to prevent silent errors that neither shows nor gets logged. */
      if (this.cspAlertById[alertId]) {
        console.log(new Error(`ErrorAlertService: alert with given id "${alertId}" is already registered.`));
        return null;
      }

      // Register with useful meta data for future alerts-keeping
      this.cspAlertById[alertId] = {
        timeCreated: Date.now(),
        alertData: alertData
      };

      /* Finally, push this alert to the stream that CSP header is subscribed to.
        This stream is hooked up in `app-header.component` as a property `alertStream` */
      this.cspAlertStream.next(alertData);

      return alertId; // Used when deregistering the alert

    }
  }


  /**
   * @param {string} alertId
   * Alert ID to deregister the alert data object from the registries. \
   * Possibly, after an actionable action has been completed. \
   * (Only applicable to CSP env.)
   */
  remove(alertId: string) {

    if (this.env.csp) {
      // Remove alert with this particular ID from CSP header's alert system
      this.cspAlertStream.next({ remove: true, id: alertId });
      if (this.cspAlertById[alertId]) { delete this.cspAlertById[alertId]; }
    }

  }

}



@Injectable()
export class GlobalErrorHandlerService implements ErrorHandler {

  constructor(
    private alert: ErrorAlertService
  ) { }

  handleError(error: Error) {
    this.alert.add(error);
  }
}
