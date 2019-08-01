/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Resolve, ActivatedRouteSnapshot } from '@angular/router';
import { Observable, from, timer, zip, of, throwError } from 'rxjs';
import { concatMap, filter, map, take, flatMap, catchError, debounceTime } from 'rxjs/operators';
import { BehaviorSubject } from 'rxjs';

import { ConsortiumService } from '../consortium/shared/consortium.service';
import { Apis } from './urls.model';

export class BlockchainRequestParams {
  consortium_id?: string;
  consortium_name: string;
  f_count: number;
  c_count: number = 0;
  deployment_type: string = 'FIXED';
  zone_ids: string[];
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
  consortium_name: string;
  node_list: Node[];
}

export interface Zone {
  id: string;
  name: string;
  latitude: number;
  longitude: number;
}

export interface BlockchainMeta {
  consortium_id: string;
  id: string;
  node_list: {
    cert: string,
    ip: string,
    node_id: string,
    url: string,
    zone_id: string,
    zone?: Zone
  }[];
}
export enum DeployStates {
  NONE = 'NONE',
  RUNNING = 'RUNNING',
  SUCCEEDED = 'SUCCEEDED',
  FAILED = 'FAILED',
}


const fakeZones: Zone[] = [{
  name: 'US West - Oregon',
  id: 'us-west',
  latitude: 0,
  longitude: 0
}, {
  name: 'US East - N Virginia',
  id: 'us-east',
  latitude: 0,
  longitude: 0
}, {
  name: 'EMEA - Frankfurt',
  id: 'emea',
  latitude: 0,
  longitude: 0
},
{
  name: 'Pacific - Sydney',
  id: 'pacific',
  latitude: 0,
  longitude: 0
}];

@Injectable({
  providedIn: 'root'
})
export class BlockchainService {
  notify: BehaviorSubject<any> = new BehaviorSubject(null);
  taskId: string;
  blockchainId: string;
  selectedBlockchain: BlockchainResponse;
  blockchains: BlockchainResponse[];
  zones: Zone[];
  zonesMap: any;
  nodesMap: any;
  metadata: any;

  constructor(
    private http: HttpClient,
    private consortiumService: ConsortiumService,
  ) { }

  deploy(params: BlockchainRequestParams): Observable<any> {
    this.notify.next({ message: 'deploying' });

    return this.consortiumService.create(params.consortium_name).pipe(
      flatMap(consort => {
        params.consortium_id = consort.consortium_id;
        return this.http.post(Apis.blockchains, params);
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
    const interval = 2500;
    const stopAfter = 6 * 60 * 1000; // 6 minutes;
    const interationAmount = stopAfter / interval;
    let iterationCount = 0;
    return timer(0, interval)
      .pipe(concatMap(() => from(this.check(taskId))))
      .pipe(filter(backendData => {
        ++ iterationCount;
        if (iterationCount >= interationAmount) {
          // tslint:disable-next-line
          throwError({message: 'Deploy timed out, please contact blockDeploying consortium timed out, please contact blockchain-support@vmware.com to report this issue.'});
        }
        return backendData.state !== DeployStates.RUNNING;
      }))
      .pipe(take(1));
  }

  set(bId?: string): Observable<BlockchainResponse[]> {
    const consortiumList = this.consortiumService.getList();
    const blockchainList = this.http.get(Apis.blockchains);
    const zoneList = this.getZones();

    return zip(consortiumList, blockchainList, zoneList)
      .pipe(
        map(response => {
          const cList = response[0] as Array<any>;
          const bList = response[1] as Array<any>;
          this.zones = response[2] as Zone[];

          cList.forEach(consortium => {
            bList.forEach(blockchain => {
              if (consortium['consortium_id'] === blockchain['consortium_id']) {
                blockchain['consortium_name'] = consortium['consortium_name'];
              }
            });
          });

          this.blockchains = JSON.parse(JSON.stringify(bList));
          this.select(bId);

          return this.blockchains;
        })
      );
  }

  async select(bId: string): Promise<boolean> {
    if (!bId) {
      this.blockchainId = null;
      return false;
    } else if (!this.isUUID(bId) && this.blockchains.length === 0) {
      this.blockchainId = undefined;
      return false;
    } else if (!this.isUUID(bId) && this.blockchains.length) {
      bId = this.blockchains[0].id;
    }

    this.blockchainId = bId;
    if (this.blockchains && this.blockchains.length) {
      this.blockchainId = this.blockchains[0].id;
      this.blockchains.forEach(bc => {
        if (bc.id === bId) {
          this.selectedBlockchain = bc;
          this.blockchainId = this.selectedBlockchain.id;
        }
      });
    }
    this.metadata = await this.getMetaData().toPromise();
    return true;
  }

  getZones(): Observable<Zone[]> {

    const refreshZones = this.http.post<Zone[]>(Apis.zonesReload, {});
    return this.http.get<Zone[]>(Apis.zones).pipe(
      map(zones => {
        const zoneMap = {};

        zones.forEach(zone => zoneMap[zone.id] = zone);
        this.zonesMap = zoneMap;

        return zones;
      }),
      catchError(error => {
        if (error.status === 500) {
          return refreshZones;
        }
      })
    );
  }

  getMetaData(): Observable<BlockchainMeta> {
    return this.http.get<BlockchainMeta>(`${Apis.blockchains}/${this.blockchainId}`)
    .pipe(
      debounceTime(1000),
      map(meta => {
        this.metadata = meta;
        const nodesMap = {};

        meta.node_list.forEach(node => {
          node.zone = this.zonesMap[node.zone_id];
          nodesMap[node.node_id] = node;
        });
        this.nodesMap = nodesMap;

        return this.metadata;
      })
    );
  }

  isUUID(uuid: string): boolean {
    return /^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i.test(uuid);
  }

}

@Injectable({
  providedIn: 'root'
})
export class BlockchainResolver implements Resolve<BlockchainResponse[]> {
  constructor(private blockchainService: BlockchainService) { }

  resolve(
    route: ActivatedRouteSnapshot
  ): Observable<BlockchainResponse[]> {
    return this.blockchainService.set(route.params['consortiumId']);
  }
}

export class BlockchainsServiceMock {
  public notify = new BehaviorSubject(null);
  public selectedBlockchain = {
    consortium_id: 1
  };
  public blockchains = [];
  public zones = fakeZones;
  public blockchaindId = 1;
  public select() {
    return true;
  }

  public getZones(): Observable<Zone[]> {
    return of(fakeZones);
  }

  public deploy(params: BlockchainRequestParams): Observable<any> {
    return of(params);
  }

}
