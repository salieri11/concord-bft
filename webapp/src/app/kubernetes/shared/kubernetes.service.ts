/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Inject, Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';

import { Kubernetes, KubernetesResponse } from './kubernetes.model';
import { GridListResponse } from '../../grid/shared/grid.model';
import { AndesApi } from '../../shared/andes-api';
import { ANDES_API_PREFIX } from '../../shared/shared.config';

@Injectable({
  providedIn: 'root'
})
export class KubernetesService extends AndesApi {

  constructor(private http: HttpClient, @Inject(ANDES_API_PREFIX) andesApiPrefix: string) {
    super(andesApiPrefix);
  }

  get apiSubPath() {
    return 'k8sclusters';
  }

  getList(params?: any): Observable<GridListResponse> {
    const options = {headers: this.headers};

    if (params) {
      options['params'] = this.buildHttpParams(params);
    }

    return this.http.get<KubernetesResponse>(this.resourcePath(), options).pipe(
      map(response => this.handleResponse(response)));
  }

  private handleResponse(response: KubernetesResponse): GridListResponse {
    if (response._embedded) {
      response._embedded.kubernetesClusters.map(kube => {
        if (kube.insecure === null) {
         kube.insecure = 'null';
        } else {
         kube.insecure = kube.insecure.toString();
        }

        if (kube.credential) {
         kube.credentialType = kube.credential.type;
        }

      });
    }
    return {
      objects: response._embedded ? response._embedded.kubernetesClusters : [] ,
      meta: {
        size: response.page ? response.page.size : 0,
        total: response.page ? response.page.totalElements : 0,
        totalPages: response.page ? response.page.totalPages : 0
      }
    };
  }

  create(org: Kubernetes): Observable<any> {
    return this.http.post<Kubernetes>(this.resourcePath(), org, {headers: this.headers});
  }

  delete(id: number): Observable<any> {
    const url = this.resourcePath(id);
    return this.http.delete(url, {headers: this.headers});
  }

  getFakeData(): Observable<GridListResponse> {
    const d = new Date(),
    data = {
      objects: [{
        id: 1,
        name: 'KubeCluseter1',
        state: 'running',
        version: '1.9',
        type: 'peer',
        created: d.setDate(d.getDate() - 5),
      }, {
        id: 1,
        name: 'KubeCluseter2',
        state: 'pending',
        version: '1.9',
        type: 'peer1',
        created: d.setDate(d.getDate() - 10),
      }, {
        id: 1,
        name: 'KubeCluseter3',
        state: 'initializing',
        version: '1.9',
        type: 'peer2',
        created: d.setDate(d.getDate() - 15),
      }],
      meta: {
        size: 10,
        total: 10,
        totalPages: 10
      }
    };

    return new Observable(observer => {
      observer.next(data);
      observer.complete();
    });
  }

}
