/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Inject, Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { TranslateService } from '@ngx-translate/core';
import { map } from 'rxjs/operators';

import { NodeProperties } from './nodes.model';
import { CONCORD_API_PREFIX } from '../../shared/shared.config';
import { ConcordApiService } from '../../shared/concord-api';
import { BlockchainService } from '../../shared/blockchain.service';

import { Apis } from '../../shared/urls.model';

@Injectable({
  providedIn: 'root'
})
export class NodesService extends ConcordApiService {

  constructor(
    @Inject(CONCORD_API_PREFIX) concordApiPrefix: string,
    private http: HttpClient,
    // @ts-ignore: no unused locals
    private blockchainService: BlockchainService,
    private translate: TranslateService
  ) {
    super(concordApiPrefix);
  }

  get apiSubPath() {
    return 'members';
  }

  getList() {
    return this.http.get<any>(`${Apis.blockchains}/${this.blockchainService.blockchainId}/replicas`).pipe(
      map(replicas => {
        const zonesMap = this.blockchainService.zonesMap;
        const groupedNodes: NodeProperties[] = [];
        const tempNode = {};

        replicas.forEach((replica) => {
          const zoneData = zonesMap[replica.zone_id];

          replica['geo'] = [Number(zoneData.longitude), Number(zoneData.latitude)];
          replica['location'] = zoneData.name;
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
            // @ts-ignore
            nodes: temp
          });
        });

        return { nodes: replicas, nodesByLocation: groupedNodes };
      })
    );
  }

  //
  // This is using the deprecated API and should be removed at some point
  //
  getNodes(): Observable<any> {
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

    return this.http.get<any>(this.resourcePath()).pipe(
      map(nodes => {
        const groupedNodes: NodeProperties[] = [];
        const tempNode = {};

        // @ts-ignore
        nodes.forEach((node, i) => {
          node['geo'] = locations[i].geo;
          node['location'] = locations[i].region;
          node['organization'] = locations[i].organization;
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
}
