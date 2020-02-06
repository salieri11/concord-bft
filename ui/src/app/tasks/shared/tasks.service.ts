/*
 * Copyright 2019-2020 VMware, all rights reserved.
 */
import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
// import { TranslateService } from '@ngx-translate/core';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
// import { BlockchainService } from '../../blockchain/shared/blockchain.service';

import { Task } from './task.model';
import { Apis } from '../../shared/urls.model';

@Injectable({
  providedIn: 'root'
})
export class TasksService {

  constructor(
    private http: HttpClient,
    // private blockchainService: BlockchainService,
    // private translate: TranslateService
  ) { }

  getList(): Observable<{ tasks: Task[]; }> {
    return this.http.get<Task[]>(Apis.tasks).pipe(
      map(tasks => {
        return {
          tasks: tasks
        };
      })
    );
  }
}
