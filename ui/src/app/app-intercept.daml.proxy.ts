/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { Observable, of } from 'rxjs';
import { HttpEvent, HttpRequest, HttpResponse, HttpClient } from '@angular/common/http';
import { damlJsonApiHeaders } from './shared/daml.utils';
import { RouteService } from './shared/route.service';

/**
 * ! Temporary
 * Until helen supports authenticated proxy passthru to DAML json-api
 * Below contains *MOCK* deploy data of DAML package
 */

const darPackage: 'quickstart' | 'quickstart2' | 'spider' = 'quickstart'; // which dar?

export enum DamlDarAvailableActions {
  extractDar = 'extractDar',
}

export function interceptDAMLCalls(request: HttpRequest<any>, http: HttpClient): Observable<HttpEvent<any>> {

  let path, blockchainId;
  const paths = request.url.split('/'); if (!paths[0]) { paths.shift(); }
  if (paths[0] === 'api' && paths[1] === 'blockchains') {
    // api/blockchains/{:uuidv4}/*
    paths.shift(); // api
    paths.shift(); // blockchains
    blockchainId = paths.shift(); // {blockchainId}
    path = paths.join('/');
  }

  let darName, darSize, contractName;
  switch (darPackage) {
    case 'quickstart2': contractName = 'DAML-quickstart2'; darName = `quickstart2.dar`; darSize = 340286; break;
    case 'spider': contractName = 'DAML-spider'; darName = `spider-modules-1.20.30.dar`; darSize = 10324375; break;
    case 'quickstart': default:
      contractName = 'DAML-quickstart'; darName = `quickstart.dar`; darSize = 344164; break;
  }
  const contractAddress = '02ab54e334149f6951910075783276664f362641094b7352191c825d709bce79';
  const contractVersion = 'version1';

  switch (path) {
    case `concord/contracts`:
      return of(new HttpResponse<any>({
        status: 200,
        body: [{
          contract_id: contractName,
          owner: 'Alice',
          type: 'DAML',
          url: `/api/blockchains/${blockchainId}/concord/contracts/${contractName}`
        }]
      }));

    case `concord/contracts/${contractName}`:
      return of(new HttpResponse<any>({
        status: 200,
        body: {
          contract_id: contractName,
          owner: 'Alice',
          type: 'DAML',
          versions: [{ address: contractAddress, version: contractVersion }],
        }
      }));

    case 'concord/contracts/daml':
      const postParams = request.body;
      return new Observable<HttpResponse<any>>(a => {
        http.post<any>(`/daml-json-api/contracts/search`, {
          '%templates': [{moduleName: postParams.moduleName, entityName: postParams.entityName}],
        }, { headers: damlJsonApiHeaders() }).subscribe(response => {
          a.next(new HttpResponse<any>({
            status: 200,
            body: response.result
          }));
        });
      });

    case `concord/contracts/${contractName}/versions/${contractVersion}`:
      return new Observable<HttpResponse<any>>(a => {
        const extractLink = RouteService.createLinkAction({
          text: 'Extract Metadata',
          action: DamlDarAvailableActions.extractDar,
          params: { contract_id: contractName, contract_version: contractVersion, }
        });

        a.next(new HttpResponse<any>({
          status: 200,
          body: {
            contract_id: contractName,
            version: contractVersion,
            address: contractAddress,
            owner: 'Alice',
            type: 'DAML',
            metadata: {
              notice: `Metadata is not extracted yet; ${extractLink}`,
              name: darName,
              size: darSize,
              compiler: 'damlc',
              output: { abi: [] },
            },
            bytecode: '',
            sourcecode: `// The source file is not displayed because the package is very large: ${extractLink}`,
            incomplete: true,
          }
        }));
      });

    case `concord/contracts/${contractName}/versions/${contractVersion}/file`:
      return new Observable<HttpResponse<Blob>>(a => {
        http.get<Blob>(getDarsFetchEndpoint(darName), { responseType: 'blob' as 'json' }).subscribe(response => {
          a.next(new HttpResponse<Blob>({ status: 200, body: response }));
        }, e => { console.log(e); });
      });
  }
  return null;
}

/**
 * ! Temporary Source of DAR file
 * Test dars should not be present in the vmwathena_blockchain codebase
 */
function getDarsFetchEndpoint(darName) {
  // Temp files server in SDDC1; until Helen supports downloading of DAR packages
  // This VM can only be accessed from VMware Internal Network
  return `https://10.72.94.44/dars/gBT9st53tQiFJKnZowFkqGcwY7dqdSBM6PvR3hNPOd2QMSzm1u/${darName}`;
}
