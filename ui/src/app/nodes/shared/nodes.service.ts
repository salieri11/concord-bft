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

@Injectable({
  providedIn: 'root'
})
export class NodesService extends ConcordApiService {

  constructor(
    @Inject(CONCORD_API_PREFIX) concordApiPrefix: string,
    private httpClient: HttpClient,
    // @ts-ignore: no unused locals
    private blockchainService: BlockchainService,
    private translate: TranslateService
  ) {
    super(concordApiPrefix);
  }

  get apiSubPath() {
    return 'members';
  }

  getNodes(): Observable<any> {
    const locations = [
      {geo: [-80.294105, 38.5976], region: 'West Virginia', organization: 'Acme Inc'},
      {geo: [-119.692793, 45.836507], region: 'Oregon', organization: 'Acme Inc'},
      {geo: [151.21, -33.868], region: 'Sydney', organization: 'On Time Dist LLC'},
      {geo: [8.67972, 45.836507], region: 'Frankfurt', organization: 'NGO'},
      {geo: [-80.294105, 38.5976], region: 'West Virginia', organization: 'Customs'},
      {geo: [-119.692793, 45.836507], region: 'Oregon', organization: 'Supplier Corp'},
      {geo: [151.21, -33.868], region: 'Sydney', organization: 'Supplier Corp'},
      {geo: [8.67972, 45.836507], region: 'Frankfurt', organization: 'Customs'},
    ];

    return this.httpClient.get<any>(this.resourcePath()).pipe(
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
