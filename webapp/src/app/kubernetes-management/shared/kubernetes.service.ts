/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders, HttpParams } from '@angular/common/http';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';

import { Kubernetes, KubernetesResponse } from './kubernetes.model';
import { GridListResponse } from '../../grid/shared/grid.model';

@Injectable({
  providedIn: 'root'
})
export class KubernetesService {
  kubeUrl = '/api/k8sclusters';
  headers: HttpHeaders = new HttpHeaders({
    'Content-Type':  'application/json',
    // 'Authorization': 'my-auth-token'
  });

  constructor(private http: HttpClient) { }

  getList(params?: any): Observable<GridListResponse> {
    const options = {headers: this.headers};

    if (params) {
      let httpParams = new HttpParams();

      for (const prop in params) {
        if (params[prop]) {
          httpParams = httpParams.set(prop, params[prop]);
        }
      }

      options['params'] = httpParams;
    }

    return this.http.get<KubernetesResponse>(this.kubeUrl, options).pipe(
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
    return this.http.post<Kubernetes>(this.kubeUrl, org, {headers: this.headers});
  }

  delete(id: number): Observable<any> {
    const url = `${this.kubeUrl}/${id}`;
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
