/*
 * Copyright 2019-2020 VMware, all rights reserved.
 */
import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
// import { TranslateService } from '@ngx-translate/core';
// import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
// import { BlockchainService } from '../../blockchain/shared/blockchain.service';

import { Feature } from './feature.model';
import { Apis } from '../../shared/urls.model';

@Injectable({
  providedIn: 'root'
})
export class FeaturesService {

  constructor(
    private http: HttpClient,
    // private blockchainService: BlockchainService,
    // private translate: TranslateService
  ) { }

  getList() {
    return this.http.get<Feature[]>(Apis.features).pipe(
      map(replicas => {

        replicas.forEach(() => {

        });

        // const onlyOnPremZones = groupedNodes.some(zone => zone.type === ZoneType.ON_PREM);

        return {
          features: replicas,
          // nodesByLocation: groupedNodes
        };
      })
    );
  }
}
