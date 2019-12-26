/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { Injectable } from '@angular/core';

interface IndexedDBObject {
  _id?: string;
  _vh?: string;
  _vc?: number;
  [key: string]: any;
}

@Injectable({
  providedIn: 'root'
})
export class IndexedDBService {

  /**
   * Using as "limitless" localStorage replacement, with key `_id`
   * IndexedDB can arbitrarily cache files up to 50% of free disk space
   * without asking user permission for the space. (LocalStorage limit: 5MB ~ 10MB)
   * https://developer.mozilla.org/en-US/docs/Web/API/IDBObjectStore
   *
   * Use Cases:
   * 1) DAR metadata caching;
   * DAR metadata & source code can be upwards of 100 MB+;
   * DAR package file is static once uploaded. (Just like ETH contracts)
   * You really don't want to re-download and extract metadata
   * every single time just to interact with a DAML-based network.
   */
  private database;

  /**
   * Define database 'default' DB collection and objectStore
   */
  async init(): Promise<any> {
    return new Promise<any>((resolve, reject) => {
      if (this.database) { return resolve(this.database); }
      const dbName = 'default';
      const request = indexedDB.open(dbName, 1);
      request.onerror = (e) => {
        console.log(e);
        reject(e);
      };
      request.onsuccess = (e) => {
        this.database = (e.target as any).result;
        resolve(this.database);
      };
      request.onupgradeneeded = (e) => {
        this.database = (e.target as any).result;
        if (this.database.objectStoreNames.length === 0) {
          const objectStore = this.database.createObjectStore('default', { keyPath: '_id' });
          // Define `_vh` hash field as unique index to prevent data race.
          objectStore.createIndex('idvh', '_vh', { unique: true });
        }
      };
    });
  }

  /**
   * Basic NoSQL CRUD on "default" objectStore (no filter condition support)
   * _id : PRIMARY KEY (optiona; auto-generated it not supplied)
   * _vc : update counter
   * _vh : unique version hash
   * */
  async add<T>(obj: IndexedDBObject): Promise<T> { // C in CRUD
    return new Promise(async (resolve) => {
      try {
        await this.init();
        if (!obj._id) { obj._id = uuidv4(); } if (!obj._vc) { obj._vc = 1; }
        obj._vh = await idvh(obj._id, obj._vc);
        const tx = this.forReadWrite().add(obj);
        tx.onsuccess = () => { resolve(obj as T); };
        tx.onerror = () => { resolve(null); };
      } catch (e) { resolve(null); }
    });
  }
  async get<T>(_id: string): Promise<T> { // R in CRUD
    return new Promise(async (resolve) => {
      try {
        await this.init();
        const tx = this.forRead().get(_id);
        tx.onsuccess = (e) => { resolve(e.target.result); };
        tx.onerror = () => { resolve(null); };
      } catch (e) { resolve(null); }
    });
  }
  async put<T>(obj: IndexedDBObject): Promise<T> { // U in CRUD
    return new Promise(async (resolve) => {
      try {
        await this.init();
        if (!obj._id) { obj._id = uuidv4(); } if (!obj._vc) { obj._vc = 1; }
        obj._vh = await idvh(obj._id, obj._vc);
        const tx = this.forReadWrite().put(obj);
        tx.onsuccess = () => { resolve(obj as T); };
        tx.onerror = () => { resolve(null); };
      } catch (e) { resolve(null); }
    });
  }
  async update<T>(_id: string, setter: (obj) => void): Promise<T> { // U in CRUD, with setter
    return new Promise(async (resolve) => {
      try {
        await this.init();
        const tx = this.forRead().get(_id); // Read
        tx.onsuccess = async (e) => {
          try {
            const obj = e.target.result as IndexedDBObject;
            if (!obj._id) { obj._id = uuidv4(); } if (!obj._vc) { obj._vc = 1; }
            setter(obj); // Update obj with supplied setter function
            ++obj._vc;
            obj._vh = await idvh(obj._id, obj._vc);
            const tx2 = this.forReadWrite().put(obj); // Write
            tx2.onsuccess = () => { resolve(obj as T); };
            tx2.onerror = () => { resolve(null); };
          } catch (e) { resolve(null); }
        };
        tx.onerror = () => { resolve(null); };
      } catch (e) { resolve(null); }
    });
  }
  async delete(_id: string): Promise<boolean> { // D in CRUD
    return new Promise(async (resolve) => {
      try {
        await this.init();
        const tx = this.forReadWrite().delete(_id);
        tx.onsuccess = () => { resolve(true); };
        tx.onerror = () => { resolve(null); };
      } catch (e) { resolve(null); }
    });
  }

  // Transactional locking
  private forRead() { return this.database.transaction('default', 'readonly').objectStore('default'); }
  private forReadWrite() { return this.database.transaction('default', 'readwrite').objectStore('default'); }
}



/** Unique data version stamping to prevent/detect concurrent write data race */
let idvhEncoder;
async function idvh(_id, _vc) {
  /**
   * ! COMPAT
   * `Crypto.subtle` is a native crypto libary in supported by all major browsers, supported in:
   * Chome 37+, IE 11+, Edge 12+, Safari(iOS) 10.3+, FF 34+, Opera 24+, Default Android Internet Browser
   * https://developer.mozilla.org/en-US/docs/Web/API/Crypto/subtle
   *
   * (Alternative: 'keccak' (SHA-3) library is already present in the codebase
   * in the unlikely chance of browser compatability becoming an issue)
   */
  if (!idvhEncoder) { idvhEncoder = new TextEncoder(); }
  const buffer = idvhEncoder.encode(_id + ' ' + _vc);
  const hashBuffer = await crypto.subtle.digest('SHA-256', buffer);
  const array = new Uint8Array(hashBuffer);
  return Array.from(array, function(byte) {
    // tslint:disable-next-line: no-bitwise
    return ('0' + (byte & 0xFF).toString(16)).slice(-2);
  }).join('');
}

/** Weak generator of uuidv4 (Math.random is weak), used for generating `_id` primary key when not supplied */
function uuidv4() {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
    // tslint:disable-next-line: no-bitwise
    const r = Math.random() * 16 | 0, v = c === 'x' ? r : (r & 0x3 | 0x8);
    return v.toString(16);
  });
}
