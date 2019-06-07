/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable, from, timer, zip } from 'rxjs';
import { concatMap, filter, map, take, flatMap } from 'rxjs/operators';
import { BehaviorSubject } from 'rxjs';

import { ConsortiumService } from '../consortium/shared/consortium.service';

export class BlockchainRequestParams {
  consortium_id?: string;
  consortium_name: string;
  f_count: number;
  c_count: number = 0;
  deployment_type: string = 'UNSPECIFIED';
}

export interface Node {
  cert: string;
  ip: string;
  node_id: string;
  region: string;
  url: string;
}

export interface BlockchainResponse {
  id: string;
  consortium_id: string;
  node_list: Node[];
  [key: string]: any;
}

@Injectable({
  providedIn: 'root'
})
export class BlockchainService {
  notify: BehaviorSubject<any> = new BehaviorSubject(null);
  taskId: string;
  blockchainId: string;
  selectedBlockchain: BlockchainResponse;
  blockchains: BlockchainResponse[];

  constructor(
    private http: HttpClient,
    private consortiumService: ConsortiumService,
  ) { }

  deploy(params: BlockchainRequestParams): Observable<any> {
    this.notify.next({ message: 'deploying' });

    return this.consortiumService.create(params.consortium_name).pipe(
      flatMap(consort => {
        params.consortium_id = consort.consortium_id;
        return this.http.post('api/blockchains', params);
      }),
      map(response => {
        this.blockchainId = response['resource_id'];
        return response;
      })
    );
  }

  getTasks(): Observable<any> {
    return this.http.get('api/tasks');
  }

  check(taskId: string): Observable<any> {
    return this.http.get(`api/tasks/${taskId}`);
  }

  pollDeploy(taskId: string): Observable<any> {
    return timer(0, 2500)
      .pipe(concatMap(() => from(this.check(taskId))))
      .pipe(filter(backendData => {
        return backendData.message !== null;
      }))
      .pipe(take(1));
  }

  set(bId?: string): Observable<BlockchainResponse[]> {
    const consortiumList = this.consortiumService.getList();
    const blockchainList = this.http.get('api/blockchains');

    return zip(consortiumList, blockchainList)
      .pipe(
        map(response => {
          const cList = response[0] as Array<any>;
          const bList = response[1] as Array<any>;

          cList.forEach(consortium => {
            bList.forEach(blockchain => {
              if (consortium['consortium_id'] === blockchain['consortium_id']) {
                blockchain['consortium_name'] = consortium['consortium_name'];
              }
            });
          });

          this.blockchains = JSON.parse(JSON.stringify(bList));

          if (this.isUUID(bId) && bId) {
            this.select(bId);
          } else {
            this.select(this.blockchains[0].id);
          }

          if (this.selectedBlockchain) {
            this.notify.next({ message: 'deployed' });
          } else {
            this.notify.next({ message: 'non-deployed' });
          }

          return this.blockchains;
        })
      );
  }

  select(bId: string): string {
    if (!this.isUUID(bId)) { return; }

    this.blockchainId = bId;

    if (this.blockchains && this.blockchains.length) {
      this.blockchains.forEach(bc => {
        if (bc.id === bId) {
          this.selectedBlockchain = bc;
        }
      });
      return bId;
    }
  }

  private isUUID(uuid: string): boolean {
    return /^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i.test(uuid);
  }

}
