import { Observable } from 'rxjs/Observable';
import {
  Input,
  ChangeDetectorRef,
  Component,
  OnInit,
  ViewChild,
} from '@angular/core';
import {
  FormArray,
  FormBuilder,
  FormGroup,
  Validators
} from '@angular/forms';
import { ActivatedRoute } from '@angular/router';
import { TranslateService } from '@ngx-translate/core';

import { ErrorAlertService } from '../../shared/global-error-handler.service';
import { GridOptions, GridColumn } from '../../grid/shared/grid.model';
import { GridComponent } from '../../grid/grid.component';
import { OrgManagementService } from '../shared/org-management.service';
import { Org } from '../shared/org-management.model';

@Component({
  selector: 'app-org-list',
  templateUrl: './org-list.component.html',
  styleUrls: ['./org-list.component.scss']
})
export class OrgListComponent implements OnInit {
  @Input('url') url: string;
  @ViewChild('grid') grid: GridComponent;
  openModalForm: boolean = false;
  modalTitle: string = '';
  formType: string;
  modalSize: string = 'md';
  gridOptions: GridOptions = new GridOptions();

  addOrgForm: FormGroup;
  addingOrg: boolean = false;
  importOrgForm: FormGroup;
  deleteOrgForm: FormGroup;
  selectedRows: Array<Org>;
  orgTypes: Array<string> = ['peer', 'orderer'];

  constructor(
    private orgService: OrgManagementService,
    private fb: FormBuilder,
    private changeDetectorRef: ChangeDetectorRef,
    private translate: TranslateService,
    private route: ActivatedRoute,
    private errorService: ErrorAlertService
  ) {
    const browserLang = translate.getBrowserLang();
    translate.setDefaultLang('en');
    translate.use(browserLang);

    this.gridOptions.getData = (params: any) => {
      return this.orgService.getList(params, this.url);
    };

    translate.get('organization.grid')
      .subscribe(grid => this.handleGrid(grid))
  }

  ngOnInit() {
    this.route.fragment.subscribe(fragment => {
        switch (fragment) {
          case "add":
            this.openAddOrg();
            break;

          default:
            // code...
            break;
        }
    });
  }


  selectedRowChange(rows: Array<Org>): void {
    this.selectedRows = rows;
  }

  openAddOrg(): void {
    this.openModal('add');
  }

  addOrg(): void {
    this.addingOrg = true;
    const formModel = this.addOrgForm.value;

    const org: Org = {
      name: formModel.name,
      peerNumber: formModel.peerNumber,
      type: formModel.type,
      domain: formModel.domain
    }

    this.orgService.create(org)
     .subscribe(
       response => this.handleAdd(response),
       error => this.handleError(error),
     );
  }

  openImportForm(): void {
    this.openModal('import');
  }

  onPrivateKeyChange(event) {
    if (event.target.files.length === 0) {
      this.importOrgForm.patchValue({
        privateKey: null
      });
      return;
    }
    const reader = new FileReader();
    reader.onload = () => {
      this.importOrgForm.patchValue({
        privateKey: reader.result
      });
      this.changeDetectorRef.markForCheck();
    };
    reader.readAsText(event.target.files[0]);
  }

  importOrg(body: any): void {
    const formModel = this.importOrgForm.value;

    const orgImport = {
      name: formModel.name,
      certificate: formModel.certificate,
      privateKey: formModel.privateKey,
    }

    this.orgService.import(orgImport)
      .subscribe(
        response => this.handleImport(response),
      );
  }

  onCertifcateChange(event) {
    if (event.target.files.length === 0) {
      this.importOrgForm.patchValue({
        certificate: null
      });
      return;
    }

    const reader = new FileReader();
    reader.onload = () => {
      this.importOrgForm.patchValue({
        certificate: reader.result
      });
      this.changeDetectorRef.markForCheck();
    };
    reader.readAsText(event.target.files[0]);
  }

  confirmDeleteOrg(id: number): void {
    this.openModal('delete');
  }

  deleteOrg(): void {
    this.selectedRows.forEach(org => {
      this.orgService.delete(org.id)
        .subscribe(
          response => this.handleDeletion(response),
          error => this.handleError(error),
        );
    })
  }

  private createAddOrgForm() {
    this.addOrgForm = this.fb.group({
      name: ['', Validators.required],
      peerNumber: ['', Validators.required],
      domain: ['', Validators.required],
      type: ['', Validators.required]
    });
  }

  private createImportOrgForm() {
    this.importOrgForm = this.fb.group({
      name: ['', Validators.required],
      certificate: [null, Validators.required],
      privateKey: [null, Validators.required]
    });
  }

  private openModal(type: string): void {
    this.formType = type;

    switch (type) {
      case "add":
        this.createAddOrgForm();
        this.modalSize = 'md';
        this.translate.get('organization.addOrgForm.title')
          .subscribe(title => this.modalTitle = title);

        break;

      case "import":
        this.modalSize = 'md';
        this.translate.get('organization.importOrgForm.title')
          .subscribe(title => this.modalTitle = title);
        this.createImportOrgForm();

        break;

      case "delete":
        this.modalSize = 'sm';
        this.translate.get('organization.deleteOrgForm.title')
          .subscribe(title => this.modalTitle = title);
        break;
    }

    this.openModalForm = true;

  }

  private handleAdd(response): void {
    console.log('add response', response);
    this.addingOrg = false;
    this.openModalForm = false;
    this.grid.addRow(response);
  }

  private handleImport(response): void {
    console.log('import response', response);
    this.openModalForm = false;
  }

  private handleDeletion(response): void {
    console.log('deletion response', response);
    this.openModalForm = false;
    this.grid.reload();
  }

  private handleError(error): void {
    console.log('handle error', error);
    this.addingOrg = false;
    this.addingOrg = false;
    this.openModalForm = false;
    this.errorService.add(error);
  }

  private handleGrid(grid: any): void {
    console.log('grid')
    console.log(grid)
    this.gridOptions.paginationTitle = grid.pagination.title;

    this.gridOptions.columns = [{
      id: 'name',
      name: grid.columns.name.title,
      type: 'string'
    }, {
      id: 'domain',
      name: grid.columns.domain.title,
      type: 'string'
    }, {
      id: 'type',
      name: grid.columns.type.title,
      type: 'string'
    }, {
      id: 'createdOn',
      name: grid.columns.created.title,
      type: 'date'
    }];
  }
}
