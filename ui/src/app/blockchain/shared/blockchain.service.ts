/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { Router } from '@angular/router';
import { Resolve, ActivatedRouteSnapshot } from '@angular/router';
import { BehaviorSubject, Observable, from, timer, zip, of, throwError } from 'rxjs';
import { concatMap, filter, map, take, flatMap, catchError } from 'rxjs/operators';
import { TranslateService } from '@ngx-translate/core';

import { ConsortiumService } from '../../consortium/shared/consortium.service';
import {
  BlockchainRequestParams,
  BlockchainResponse,
  Zone,
  OnPremZone,
  BlockchainMeta,
  DeployStates,
  fakeZones,
  ContractEngines
} from './blockchain.model';
import { Apis } from '../../shared/urls.model';

@Injectable({
  providedIn: 'root'
})
export class BlockchainService {
  taskId: string;
  blockchainId: string;
  selectedBlockchain: BlockchainResponse;
  blockchains: BlockchainResponse[];
  zones: Zone[];
  zonesMap: any;
  nodesMap: any;
  metadata: any;
  type: ContractEngines;

  notify: BehaviorSubject<any> = new BehaviorSubject(null);

  constructor(
    private http: HttpClient,
    private consortiumService: ConsortiumService,
    private translateService: TranslateService
  ) { }

  deploy(params: BlockchainRequestParams): Observable<any> {
    this.notify.next({ message: 'deploying', type: params.blockchain_type });

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
    const stopAfter = 10 * 60 * 1000; // 10 minutes;
    const interationAmount = stopAfter / interval;
    let iterationCount = 0;
    return timer(0, interval)
      .pipe(concatMap(() => from(this.check(taskId))))
      .pipe(filter(backendData => {
        ++iterationCount;
        if (iterationCount >= interationAmount) {
          // tslint:disable-next-line
          throwError({ message: this.translateService.instant('error.timeout') });
        }
        return backendData.state !== DeployStates.RUNNING;
      }))
      .pipe(take(1));
  }

  set(bId?: string): Observable<boolean> {
    const consortiumList = this.consortiumService.getList();
    const blockchainList = this.http.get(Apis.blockchains);
    const zoneList = this.getZones();

    return zip(consortiumList, blockchainList, zoneList)
      .pipe(
        map((response) => {
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

          this.blockchains = bList;

          return this.blockchains;
        }),
        flatMap(() => this.select(bId ? bId : this.blockchainId))
      );
  }

  select(bId: string): Observable<boolean> {
    if (!bId) {
      this.blockchainId = null;
      return of(false);
    } else if (!this.isUUID(bId) && this.blockchains.length === 0) {
      this.blockchainId = undefined;
      return of(false);
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
    return this.getMetaData().pipe(
      map(metadata => {
        this.metadata = metadata;
        return true;
      }),
    );
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
        map(meta => {
          this.metadata = meta;
          const nodesMap = {};
          this.type = meta.blockchain_type;
          meta.node_list.forEach(node => {
            node.zone = this.zonesMap[node.zone_id];
            nodesMap[node.node_id] = node;
          });
          this.nodesMap = nodesMap;

          return this.metadata;
        })
      );
  }

  addOnPremZone(zone: OnPremZone): Observable<any> {
    return this.http.post<OnPremZone>(Apis.zones, zone).pipe(
      map(onPremZone => {
        this.zones.push(onPremZone);

        // TODO - refresh store when a new zone is added
        // this.set(null).pipe(
        //   catchError(error => {
        //     this.router.navigate(['error'], {
        //       queryParams: { error: JSON.stringify(error) }
        //     });

        //     return error;
        //   })
        // );

        return onPremZone;
      }),
      catchError(error => {
        return error.message;
      })
    );
  }

  testOnPremZoneConnection(zone: Zone): Observable<Zone> {
    return this.http.post<OnPremZone>(Apis.zonesTextConnection, zone);
  }

  getZoneLatLong(name: string): Observable<any> {
    const params = new HttpParams().set(
      'key', '349062d268624582b19e6a25d8a3fd60').set(
        'q', name);

    // const params = {};
    return this.http.get('/geo', { params: params }).pipe(
      // @ts-ignore
      map<{ results: any[] }>(locations => {
        const newLocations = [];

        locations.results.forEach(loc => {
          newLocations.push({
            displayValue: loc.formatted,
            value: `${loc.formatted}`,
          });
        });

        return newLocations;
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
export class BlockchainResolver implements Resolve<boolean> {

  constructor(
    private blockchainService: BlockchainService,
    private router: Router
  ) { }

  resolve(
    route: ActivatedRouteSnapshot
  ): Observable<boolean | any> {
    return this.blockchainService.set(route.params['consortiumId']).pipe(
      catchError(error => {
        this.router.navigate(['error'], {
          queryParams: { error: JSON.stringify(error) }
        });

        return error;
      })
    );
  }
}

export class BlockchainsServiceMock {
  public notify = new BehaviorSubject({ message: '', type: '' });
  public selectedBlockchain = {
    consortium_id: 1
  };
  public blockchains = [];
  public zones = fakeZones;
  public blockchaindId = 1;
  public type = ContractEngines.ETH;

  public select(id: string): Observable<boolean> {
    return of(typeof id === 'string');
  }

  public getZones(): Observable<Zone[]> {
    return of(fakeZones);
  }

  public deploy(params: BlockchainRequestParams): Observable<any> {
    return of(params);
  }

}
