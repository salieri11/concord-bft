/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { TranslateService } from '@ngx-translate/core';
import { map } from 'rxjs/operators';

import { NodeProperties, NodeInfo } from './nodes.model';
import { ZoneType } from './../../zones/shared/zones.model';
import { BlockchainService } from '../../blockchain/shared/blockchain.service';

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

  constructor(
    private http: HttpClient,
    private blockchainService: BlockchainService,
    private translate: TranslateService
  ) { }

  getList() {
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

  //
  // This is using the deprecated API and should be removed at some point
  //
  getNodes(): Observable<any> {
    return this.http.get<any>(Apis.members(this.blockchainService.blockchainId)).pipe(
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
