import { Routes } from "@angular/router";
import { SmartContractsContainerComponent } from "./smart-contracts-container/smart-contracts-container.component";
import { SmartContractDetailContainerComponent } from "./smart-contract-detail-container/smart-contract-detail-container.component";

export const smartContractRoutes: Routes = [
  { path: '', component: SmartContractsContainerComponent },
  {
    path: ':contractId',
    component: SmartContractDetailContainerComponent
  },
  {
    path: ':contractId/versions/:version',
    component: SmartContractDetailContainerComponent
  }
];
