/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { BehaviorSubject, Observable, from, timer, zip, of, throwError } from 'rxjs';
import { concatMap, filter, map, take, catchError, mergeMap } from 'rxjs/operators';
import { TranslateService } from '@ngx-translate/core';

import { ConsortiumService } from '../../consortium/shared/consortium.service';
import { ConsortiumResponse } from '../../consortium/shared/consortium.model';
import {
  BlockchainRequestParams,
  BlockchainResponse,
  DeployStates,
  ContractEngines,
  TempDeployTracker,
  TempDeployTrackerRegistry,
  BlockchainStates,
} from './blockchain.model';

import { Zone, fakeZones } from './../../zones/shared/zones.model';
import { Apis, uuidRegExp } from '../../shared/urls.model';

@Injectable({
  providedIn: 'root'
})
export class BlockchainService {
  taskId: string;
  blockchainId: string;
  selectedBlockchain: BlockchainResponse;

  blockchains: BlockchainResponse[];
  inactiveBlockchains: BlockchainResponse[];
  failedBlockchains: BlockchainResponse[];

  zones: Zone[];
  zonesMap: {[zone_id: string]: Zone};
  metadata: any;
  type: ContractEngines;

  notify: BehaviorSubject<any> = new BehaviorSubject(null);
  canDeploy: BehaviorSubject<boolean> = new BehaviorSubject(null);

  constructor(
    private http: HttpClient,
    private consortiumService: ConsortiumService,
    private translateService: TranslateService,
  ) { }

  deploy(params: BlockchainRequestParams, isOnlyOnPrem: boolean): Observable<any> {
    this.notify.next({
      message: 'deploying',
      type: params.blockchain_type,
      isOnlyOnPrem: isOnlyOnPrem
    });

    const deployRequestTime = Date.now();
    const tempKey = 'unassigned_' + deployRequestTime;
    const tracker: TempDeployTracker = { key: tempKey, create_params: params, requested: deployRequestTime };
    this.saveDeployingData(tracker);

    return this.consortiumService.create(params.consortium_name).pipe(
      mergeMap(consort => {
        params.consortium_id = consort.consortium_id;
        const deployData = this.loadDeployingData(tempKey);
        deployData.responded = Date.now();
        deployData.consortium_id = consort.consortium_id;
        deployData.consortium_name = consort.consortium_name;
        deployData.organization_id = consort.organization_id;
        this.saveDeployingData(deployData);
        return this.http.post(Apis.blockchains, params);
      }),
      map(response => {
        const taskId = response['task_id'];
        if (taskId) {
          const deployRegistry = this.loadDeployingRegistry();
          deployRegistry[tempKey].responded_task = Date.now();
          deployRegistry[tempKey].task_id = taskId;
          // change 'unassigned' to known taskId on registry, delete temp key
          deployRegistry[taskId] = deployRegistry[tempKey];
          deployRegistry[taskId].key = taskId;
          delete deployRegistry[tempKey];
          this.saveDeployingRegistry(deployRegistry);
          this.blockchainId = deployRegistry[taskId].consortium_id;
        }
        return response;
      })
    );
  }

  get noConsortiumJoined(): boolean {
    return (!this.blockchains || this.blockchains.length === 0);
  }

  getTasks(): Observable<any> {
    return this.http.get(Apis.tasks);
  }

  getTask(taskId: string): Observable<any> {
    return this.http.get(`${Apis.tasks}/${taskId}`);
  }

  pollDeploy(taskId: string): Observable<any> {
    const interval = 2500;
    const stopAfter = 10 * 60 * 1000; // 10 minutes;
    const interationAmount = stopAfter / interval;
    let iterationCount = 0;
    return timer(0, interval)
      .pipe(concatMap(() => from(this.getTask(taskId))))
      .pipe(filter(backendData => {
        ++iterationCount;
        if (iterationCount >= interationAmount) {
          // tslint:disable-next-line
          throwError({ message: this.translateService.instant('error.timeout') });
        }

        const completed = backendData.state !== DeployStates.RUNNING;
        const failed = (backendData.state === DeployStates.FAILED);
        if (completed) {
          const deployRegistry = this.loadDeployingRegistry();
          if (!failed) {
            delete deployRegistry[taskId]; // succeeded, remove temp deploy object
          } else {
            deployRegistry[taskId].state = DeployStates.FAILED;
          }
          this.saveDeployingRegistry(deployRegistry);
        }

        return completed;
      }))
      .pipe(take(1));
  }

  set(blockchainId?: string): Observable<boolean> {
    const consortiumList = this.consortiumService.getList();
    const blockchainList = this.http.get(Apis.blockchains);
    const zoneList = this.getZones();
    return zip(consortiumList, blockchainList, zoneList)
      .pipe(
        map((response) => {
          const cList = response[0] as Array<ConsortiumResponse>;
          const bList = response[1] as Array<BlockchainResponse>;
          this.zones = response[2] as Zone[];
          cList.forEach(consortium => {
            bList.forEach(blockchain => {
              if (consortium.consortium_id === blockchain.consortium_id) {
                blockchain.consortium_name = consortium.consortium_name;
              }
            });
          });

          // Classify based on current state
          this.blockchains = bList.filter(item => item.blockchain_state === BlockchainStates.ACTIVE);
          this.inactiveBlockchains = bList.filter(item => item.blockchain_state === BlockchainStates.FAILED);
          this.failedBlockchains = bList.filter(item => item.blockchain_state === BlockchainStates.FAILED);

          return this.blockchains;
        }),
        mergeMap(() => this.select(blockchainId))
      );
  }

  select(blockchainId?: string): Observable<boolean> {
    this.blockchainId = null;
    this.selectedBlockchain = null;
    if (!this.blockchains || this.blockchains.length === 0) { return of(false); }
    this.blockchains.forEach(blockchain => { // check blockchain actually exists on list
      if (blockchain.id === blockchainId) {
        this.selectedBlockchain = blockchain;
        this.blockchainId = this.selectedBlockchain.id;
      }
    });
    if (!this.selectedBlockchain) { return of(false); }
    this.type = this.selectedBlockchain.blockchain_type;
    this.saveSelectedBlockchain(blockchainId);
    return of(true);
  }

  getZones(): Observable<Zone[]> {
    const refreshZones = this.http.post<Zone[]>(Apis.zonesReload, {});
    return this.http.get<Zone[]>(Apis.zones).pipe(
      map(zones => {
        const zoneMap = {};
        zones.forEach(zone => zoneMap[zone.id] = zone);
        this.zonesMap = zoneMap;
        this.zones = zones;
        return zones;
      }),
      catchError(error => {
        if (error.status === 500) {
          return refreshZones;
        }
      })
    );
  }

  isUUID(uuid: string): boolean {
    return uuidRegExp.test(uuid);
  }

  saveDeployingData(deployData: TempDeployTracker) {
    const data = this.getDeployingRegistry();
    data[deployData.key] = deployData;
    localStorage.setItem('deployingNow', JSON.stringify(data));
  }
  saveDeployingRegistry(deployDataRegistry: TempDeployTrackerRegistry) {
    localStorage.setItem('deployingNow', JSON.stringify(deployDataRegistry));
  }
  loadDeployingData(key: string): TempDeployTracker {
    const data = this.getDeployingRegistry();
    return data[key];
  }
  loadDeployingRegistry(): TempDeployTrackerRegistry {
    return this.getDeployingRegistry();
  }

  saveSelectedBlockchain(blockchainId: string) {
    localStorage.setItem('selectedBlockchain', blockchainId);
  }

  loadSelectedBlockchain() {
    return localStorage.getItem('selectedBlockchain');
  }

  private getDeployingRegistry(): TempDeployTrackerRegistry {
    const data = localStorage.getItem('deployingNow');
    if (!data) {
      return {} as TempDeployTrackerRegistry;
    } else {
      try { return JSON.parse(data) as TempDeployTrackerRegistry; } catch (e) { return null; }
    }
  }

}


export class MockBlockchainsService {
  notify = new BehaviorSubject({ message: '', type: '' });
  canDeploy = new BehaviorSubject(true);
  selectedBlockchain = { consortium_id: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa' };
  blockchains = [];
  zones = fakeZones;
  blockchaindId = 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa';
  type = ContractEngines.ETH;
  metadata = {consortium_id: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa'};

  select(id: string): Observable<boolean> {
    return of(typeof id === 'string');
  }

  getZones(): Observable<Zone[]> {
    return of(fakeZones);
  }

  deploy(params: BlockchainRequestParams): Observable<any> {
    return of(params);
  }

  saveSelectedConsortium(consortiumId: string) {
    localStorage.setItem('selectedConsortium', consortiumId);
  }

  loadSelectedConsortium() {
    return localStorage.getItem('selectedConsortium');
  }

}
