/*
 * Copyright 2018 VMware, all rights reserved.
 */
export interface KubernetesCredential {
  type: string;
  value: {
    username?: string,
    password?: string,
    private_key?: string;
    certificate?: string;
    file?: string;
  };
}

export interface KubernetesForm {
  id?: number;
  name: string;
  apiServerUrl?: string;
  credential?: KubernetesCredential;
}

export interface Kubernetes {
  id?: number;
  name: string;
  state?: string;
  version?: string;
  apiServerUrl?: string;
  insecure?: any;
  credentialType?: string;
  credential: KubernetesCredential;
  helmCliUrl?: string;
  kubectlCliUrl?: string;

  createdOn?: number;
  updatedOn?: number;
}

export interface KubernetesResponse {
  _embedded: {
    kubernetesClusters: Array<Kubernetes>,
    _links: any,
  };
  page: {
    size: number,
    totalElements: number,
    totalPages: number
  };
}
