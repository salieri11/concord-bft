/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { map, catchError } from 'rxjs/operators';

import { Observable } from 'rxjs';

import { Zone, OnPremZone, ZoneDependencies } from './zones.model';
import { Apis } from '../../shared/urls.model';

@Injectable({
  providedIn: 'root'
})
export class ZonesService {

  constructor(private http: HttpClient) { }

  getZone(id: string): Observable<OnPremZone> {
    return this.http.get<OnPremZone>(`${Apis.zones}/${id}`);
  }

  getZones(): Observable<Zone[]> {

    const refreshZones = this.http.post<Zone[]>(Apis.zonesReload, {});
    return this.http.get<Zone[]>(Apis.zones).pipe(
      catchError(error => {
        if (error.status === 500) {
          return refreshZones;
        }
      })
    );
  }

  update(id: string, zone: OnPremZone): Observable<any> {
    return this.http.patch<OnPremZone>(`${Apis.zones}/${id}`, zone);
  }

  delete(id: string): Observable<any> {
    return this.http.delete<OnPremZone>(`${Apis.zones}/${id}`);
  }

  addZone(zone: OnPremZone): Observable<any> {
    return this.http.post<OnPremZone>(Apis.zones, zone);
  }

  testOnPremZoneConnection(zone: Zone): Observable<Zone> {
    return this.http.post<OnPremZone>(Apis.zonesTestConnection, zone);
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
            geometry: {
              lat: loc.geometry.lat,
              long: loc.geometry.lng
            }
          });
        });

        return newLocations;
      })
    );
  }

  dependencies(zone_id: string): Observable<ZoneDependencies> {
    return this.http.get<ZoneDependencies>(`${Apis.zonesDep}/${zone_id}`);
  }

  canDelete(zone_id: string): Observable<boolean> {
    return this.dependencies(zone_id).pipe(
        map(res => res.replica_list.length === 0 && res.client_list.length === 0)
    );
  }
}
