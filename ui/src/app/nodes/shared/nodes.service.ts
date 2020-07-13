/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
// import { FormData } from '@angular/forms';
import { TranslateService } from '@ngx-translate/core';
import { Observable, from, timer, of, Subject, zip } from 'rxjs';
import { map, concatMap, filter, take, delay } from 'rxjs/operators';
import { VmwTasksService, VmwTask, VmwTaskState, IVmwTaskInfo } from '../../shared/components/task-panel/tasks.service';

import { NodeProperties, NodeInfo, ClientNode, ClientNodeDeployParams,
        CommittersData, BlockchainNode, NodeCredentials } from './nodes.model';
import { ZoneType } from './../../zones/shared/zones.model';
import { BlockchainService } from '../../blockchain/shared/blockchain.service';
import { DeployStates } from '../../blockchain/shared/blockchain.model';

import { Apis } from '../../shared/urls.model';

// Fr testing and rendering map for local deployments
const locations = [
  { geo: [-80.294105, 38.5976], region: 'West Virginia', organization: 'Acme Inc' },
  { geo: [-119.692793, 45.836507], region: 'Oregon', organization: 'Acme Inc' },
  { geo: [151.21, -33.868], region: 'Sydney', organization: 'On Time Dist LLC' },
  { geo: [8.67972, 45.836507], region: 'Frankfurt', organization: 'NGO' },
  { geo: [-80.294105, 38.5976], region: 'West Virginia', organization: 'Customs' },
  { geo: [-119.692793, 45.836507], region: 'Oregon', organization: 'Supplier Corp' },
  { geo: [151.21, -33.868], region: 'Sydney', organization: 'Supplier Corp' },
  { geo: [8.67972, 45.836507], region: 'Frankfurt', organization: 'Customs' },
];

@Injectable({
  providedIn: 'root'
})
export class NodesService {
  tasks: {[key: string]: {taskDetails?: IVmwTaskInfo, trackedTask?: VmwTask}} = {};
  private _changedSubject: Subject<VmwTask> = new Subject();
  tasksUpdated: Observable<any> = this._changedSubject.asObservable();
  clients: ClientNode[] = [];
  committers: NodeInfo[] = [];
  committersData: CommittersData = null;
  allNodesList: BlockchainNode[] = [];

  onNodeList: Subject<boolean> = new Subject();

  committersWithOnlyPublicIP: boolean = false;
  committersWithOnlyPrivateIP: boolean = false;
  clientsWithOnlyPublicIP: boolean = false;
  clientsWithOnlyPrivateIP: boolean = false;
  committersWithoutRPCURL: boolean = false;
  clientsWithoutRPCURL: boolean = false;
  committersWithNoZoneInfo: boolean = false;

  constructor(
    private http: HttpClient,
    private blockchainService: BlockchainService,
    private taskService: VmwTasksService,
    private translate: TranslateService
  ) { }

  getList(): Observable<CommittersData> {
    return this.http.get<NodeInfo[]>(Apis.nodes(this.blockchainService.blockchainId)).pipe(
      map(replicas => {
        const zonesMap = this.blockchainService.zonesMap;
        const groupedNodes: NodeProperties[] = [];
        const tempNode = {};

        replicas.forEach((replica, i) => {
          let zoneData = zonesMap[replica.zone_id];

          if (!zoneData) {
            const loc = locations[i];
            zoneData = {longitude: loc.geo[0], latitude: loc.geo[1], name: loc.region};
          }

          replica.geo = [Number(zoneData.longitude), Number(zoneData.latitude)];
          replica.location = zoneData.name;
          replica.zone_type = zoneData.type;
          replica['state'] = replica['status'];

          // Fake Single location
          // replica['location'] = 'Palo Alto CA USA';
          // replica['geo'] = [-122.143936,37.468319]

          let text = '';
          let labelClass = '';

          if (replica.millis_since_last_message < replica.millis_since_last_message_threshold) {
            text = this.translate.instant('nodes.healthy');
            labelClass = 'label-success';
            replica['healthy'] = true;
            replica['status'] = text;
          } else {
            text = this.translate.instant('nodes.unhealthy');
            labelClass = 'label-danger';
            replica['healthy'] = false;
            replica['status'] = text;
          }

          // This is useful for testing random health in the UI
          // if (Math.random() >= 0.5) {
          //   text = this.translate.instant('replicas.unhealthy');
          //   labelClass = 'label-danger';
          //   replica['healthy'] = false;
          //   replica['status'] = text;
          // }

          replica['healthHTML'] = `<div class="label-container"><span class="label ${labelClass}">${text}</span></div>`;

          //
          // Cluster replicas
          if (tempNode[replica.location]) {
            tempNode[replica.location].push(replica);
          } else {
            tempNode[replica.location] = [replica];
          }

        });

        Object.values(tempNode).forEach(temp => {
          groupedNodes.push({
            location: temp[0].location,
            geo: temp[0].geo,
            type: temp[0].type,
            // @ts-ignore
            nodes: temp
          });
        });

        const onlyOnPremZones = groupedNodes.some(zone => zone.type === ZoneType.ON_PREM);

        return {
          nodes: replicas,
          nodesByLocation: groupedNodes,
          onlyOnPrem: onlyOnPremZones
        };
      })
    );
  }


  getClients(): Observable<ClientNode[]> {
    const zonesMap = this.blockchainService.zonesMap;
    return this.http.get<ClientNode[]>(
      Apis.clients(this.blockchainService.blockchainId)
    ).pipe(
      map(response => {
        // Add zone name
        response.forEach((client, i) => {
          if (zonesMap[client.zone_id]) {
            client['zone_name'] = zonesMap[client.zone_id].name;
          } else {
            client['zone_name'] = undefined;
          }
          if (!client['name']) {
            client['name'] = 'Client' + (i + 1);
          }
        });
        return response;
      })
    );
  }

  getNodeCredentials(nodeId: string): Observable<NodeCredentials> {
    return this.http.get<NodeCredentials>(
      Apis.nodeCredentials(this.blockchainService.blockchainId, nodeId)
    );
  }

  getAllNodeTypes(): Observable<any[]> {
    return zip(this.getList(), this.getClients())
      .pipe(
        map(response => {
          let nodes = [];
          const clients = response[1];
          clients.forEach(cl => {
            cl.name = cl.host_name ? cl.host_name : cl.name;
          });
          nodes = nodes.concat(response[0].nodes);
          nodes = nodes.concat(clients);
          // Merge client and replica nodes together in one array
          return nodes;
        })
       );
  }

  /**
   * @deprecated by V2 API; dynamic adding of clients deprecated.
  */
  deployClients(params: ClientNodeDeployParams) {
    this.http.post<any>(Apis.clients(this.blockchainService.blockchainId), params)
    .subscribe(response => {
      this.pollUntilDeployFinished(response['task_id'], name);
    });
  }

  pollDeploy(taskId: string): Observable<any> {
    const interval = 2500;

    return timer(0, interval)
      .pipe(concatMap(() => from(this.blockchainService.getTask(taskId))))
      .pipe(filter(backendData => {
        const completed = backendData.state !== DeployStates.RUNNING;
        const failed = (backendData.state === DeployStates.FAILED);

        return completed || failed;
      }))
      .pipe(take(1));
  }

  refreshAllNodesList(): Observable<any> {
    const committersDataObservable = this.getList();
    const clientsObservable = this.getClients();
    return zip(committersDataObservable, clientsObservable).pipe(
      map(r => {
        const committersData = r[0] as CommittersData;
        const clients = r[1];
        this.allNodesList = [];
        this.committersData = committersData;
        this.committers = committersData.nodes;
        this.clients = clients;
        this.committers.forEach((committer, i) => {
          const committerNameI18N = this.translate.instant('nodes.committer');
          committer.name = this.trimNodeName(committerNameI18N, committer, i);
          committer.name_ordinal = this.trimNodeName(committerNameI18N, null, i);
        });
        this.clients.forEach((client, i) => {
          const clientNameI18N = this.translate.instant('nodes.client');
          client.name = this.trimNodeName(clientNameI18N, client, i);
          client.name_ordinal = this.trimNodeName(clientNameI18N, null, i);
        });
        this.committersWithOnlyPublicIP = true;
        this.committersWithOnlyPrivateIP = true;
        this.committersWithoutRPCURL = true;
        this.committersWithNoZoneInfo = true;
        for (const committer of committersData.nodes) {
          committer.node_type = 'committer';
          this.allNodesList.push(committer);
          if (committer.public_ip) { this.committersWithOnlyPrivateIP = false; }
          if (committer.private_ip) { this.committersWithOnlyPublicIP = false; }
          if (committer.rpc_url) { this.committersWithoutRPCURL = false; }
        }
        this.clientsWithOnlyPublicIP = true;
        this.clientsWithOnlyPrivateIP = true;
        this.clientsWithoutRPCURL = true;
        for (const client of clients) {
          client.node_type = 'client';
          this.allNodesList.push(client);
          if (client.public_ip) { this.clientsWithOnlyPrivateIP = false; }
          if (client.private_ip) { this.clientsWithOnlyPublicIP = false; }
          if (client.url) { this.clientsWithoutRPCURL = false; }
        }
        this.committersWithNoZoneInfo = true;
        this.allNodesList.forEach(node => {
          const zone = this.blockchainService.zonesMap[node.zone_id];
          if (zone) {
            if (node.node_type === 'committer') { this.committersWithNoZoneInfo = false; }
            node['zone'] = zone;
            node['zone_name'] = zone.name;
          }
        });
        this.onNodeList.next(true);
      }),
    );
  }

  private trimNodeName(prefix: string, node: BlockchainNode, index: number) {
    if (!node || !node.name || node.name.startsWith('null')) {
      return prefix + ' ' + (index + 1);
    }
    return node.name;
  }

  private startNotification(taskId: string, name: string): Observable<any> {
    const currentTask = this.tasks[taskId] = {taskDetails: null, trackedTask: null};
    currentTask.taskDetails = {
      title: this.translate.instant('nodes.deployingClient'),
      description: `${this.translate.instant('nodes.deployingClientDesc')} ${name}`,
      progress: 10
    };

    currentTask.trackedTask = this.taskService.trackTask(currentTask.taskDetails);

    const multiplier = 5;
    const duration = 1000 * multiplier;

    return of(true).pipe(
      delay(duration),
      map(response => {
        currentTask.taskDetails.progress = 10;
        return response;
      }),
      delay(duration),
      map(() => {currentTask.taskDetails.progress = 11; }),
      delay(duration / 2),
      map(() => {currentTask.taskDetails.progress = 14; }),
      delay(duration / 2),
      map(() => {currentTask.taskDetails.progress = 19; }),
      delay(duration),
      map(() => {
        currentTask.taskDetails.progress = 25;
      }),
      delay(duration),
      map(() => {currentTask.taskDetails.progress = 31; }),
      delay(duration),
      map(() => {currentTask.taskDetails.progress = 34; }),
      delay(duration),
      map(() => {currentTask.taskDetails.progress = 34; }),
      delay(duration / 2),
      map(() => {currentTask.taskDetails.progress = 38; }),
      delay(duration / 2),
      map(() => {currentTask.taskDetails.progress = 40; }),
      delay(duration / 2),
      map(() => {
        currentTask.taskDetails.progress = 41;
      }),
      delay(duration),
      map(() => {currentTask.taskDetails.progress = 43; }),
      delay(duration),
      map(() => {
        currentTask.taskDetails.progress = 45;
      }),
      delay(duration),
      map(() => {currentTask.taskDetails.progress = 45; }),
      delay(duration / 2),
      map(() => {currentTask.taskDetails.progress = 50; }),
      delay(duration / 2),
      map(() => {currentTask.taskDetails.progress = 53; }),
      delay(duration),
      map(() => {currentTask.taskDetails.progress = 60; }),
      delay(duration / 2),
      map(() => {currentTask.taskDetails.progress = 62; }),
      delay(duration / 2),
      map(() => {currentTask.taskDetails.progress = 72; }),
      delay(duration / 2),
      map(() => {currentTask.taskDetails.progress = 80; }),
      delay(duration),
      map(() => {currentTask.taskDetails.progress = 85; }),
      delay(duration),
    );

  }

  private pollUntilDeployFinished(taskId: string, name: string) {
    const message = this.startNotification(taskId, name).subscribe();

    this.pollDeploy(taskId).subscribe(response => {
      const currentTask = this.tasks[taskId];
      this._changedSubject.next(response);

      switch (response.state) {
        case DeployStates.SUCCEEDED:
          message.unsubscribe();
          currentTask.taskDetails.progress = 100;
          currentTask.trackedTask.state = VmwTaskState.COMPLETED;
          currentTask.taskDetails.description = this.translate.instant('nodes.deployClientSuccess');
          break;

        case DeployStates.FAILED:
          message.unsubscribe();
          currentTask.taskDetails.progress = 0;
          currentTask.trackedTask.state = VmwTaskState.ERROR;
          currentTask.taskDetails.description = `${this.translate.instant('nodes.deployClientFailed')} ${taskId}`;
          break;
      }
    });
  }


  //
  // This is using the deprecated API and should be removed at some point
  //
  getNodes(): Observable<any> {
    return this.http.get<any>(Apis.nodes(this.blockchainService.blockchainId)).pipe(
      map(nodes => {
        const groupedNodes: NodeProperties[] = [];
        const tempNode = {};

        // @ts-ignore
        nodes.forEach((node, i) => {
          node['geo'] = locations[i].geo;
          node['location'] = locations[i].region;
          node['organization'] = locations[i].organization;
          node['state'] = node['status'];
          let text = '';
          let labelClass = '';

          if (node.millis_since_last_message < node.millis_since_last_message_threshold) {
            text = this.translate.instant('nodes.healthy');
            labelClass = 'label-success';
            node['healthy'] = true;
            node['status'] = text;
          } else {
            text = this.translate.instant('nodes.unhealthy');
            labelClass = 'label-danger';
            node['healthy'] = false;
            node['status'] = text;
          }

          // This is for testing random health
          // if (Math.random() >= 0.5) {
          //   text = this.translate.instant('nodes.unhealthy');
          //   labelClass = 'label-danger';
          //   node['healthy'] = false;
          //   node['status'] = text;
          // }

          node['healthHTML'] = `<div class="label-container"><span class="label ${labelClass}">${text}</span></div>`;

          //
          // Cluster nodes
          if (tempNode[node.location]) {
            tempNode[node.location].push(node);
          } else {
            tempNode[node.location] = [node];
          }

        });

        Object.values(tempNode).forEach(temp => {
          groupedNodes.push({
            location: temp[0].location,
            geo: temp[0].geo,
            // @ts-ignore
            nodes: temp
          });
        });

        return { nodes: nodes, nodesByLocation: groupedNodes };
      })
    );
  }

  // action(action: string, node: any): Observable<any> {
  //   if (node.length) {
  //     const actions = [];

  //     node.forEach(n => {
  //       actions.push(
  //         this.http.post(Apis.getReplica(n.id), { action: action })
  //       );
  //     });

  //     return forkJoin(actions).pipe(
  //         mergeMap(action => this.checkReplicaTask(action['task_id']))
  //       );
  //   } else {
  //     return this.http.post(
  //         Apis.getReplica(node.id), { action: action }
  //       ).pipe(
  //         mergeMap(action => this.checkReplicaTask(action['task_id']))
  //       );
  //   }

  // }

  // private checkReplicaTask(taskId: string): Observable<any> {
  //   timer(0, interval)
  //     .pipe(concatMap(() => from(this.http.get(`api/tasks/${taskId}`))))
  //     .pipe(filter(task => task['state'] !== 'RUNNING'))
  //     .pipe(take(1));
  // }

}
