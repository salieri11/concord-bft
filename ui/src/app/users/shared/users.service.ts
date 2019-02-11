/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs/index';

import { User } from './user.model';
import { EthWallet } from '../../shared/eth-api.model';

@Injectable({
  providedIn: 'root'
})
export class UsersService {

  constructor(private http: HttpClient) {

  }

  path = 'api/users';
  users: User[] = [];

  getList(): Observable<User[]> {
    return this.http.get<User[]>(this.path);
  }

  createUser(user: User): Observable<any> {
    return this.http.post<User>(this.path, user);
  }

  deleteUser(userId: number): Observable<any> {
    const url = `${this.path}/${userId}`;
    return this.http.delete(url);
  }

  editUser(user: User) {
    const url = `${this.path}/${user.user_id}`;
    return this.http.patch<User>(url, user);
  }

  getUser(id: string) {
    const url = `${this.path}/${id}`;
    return this.http.get<User>(url);
  }

  getUserWallets(id: string) {
    const url = `${this.path}/${id}/wallet`;
    return this.http.get<string[]>(url);
  }

  getUserWalletByAddress(id: string, walletAddress: string) {
    const url = `${this.path}/${id}/wallet/${walletAddress}`;
    return this.http.get<EthWallet>(url);
  }
}
