/*
 * Copyright 2018 VMware, all rights reserved.
 */
import {
  ChangeDetectorRef,
  Component,
  OnInit,
  ViewChild,
} from '@angular/core';
import {
  FormBuilder,
  FormGroup,
  Validators,
} from '@angular/forms';
import { TranslateService } from '@ngx-translate/core';

import { GridOptions } from '../grid/shared/grid.model';
import { GridComponent } from '../grid/grid.component';
import { ConsortiumService } from './shared/consortium.service';
import { Consortium } from './shared/consortium.model';


@Component({
  selector: 'app-consortium',
  templateUrl: './consortium.component.html',
  styleUrls: ['./consortium.component.scss']
})
export class ConsortiumComponent implements OnInit {
  @ViewChild('grid') grid: GridComponent;
  openModalForm = false;
  modalTitle = '';
  formType: string;
  modalSize = 'md';
  gridOptions: GridOptions = new GridOptions();

  addConsortiumForm: FormGroup;
  get credentialType(): any {
    return this.addConsortiumForm.get('credentialType');
  }
  deleteConsortiumForm: FormGroup;
  selectedRows: Array<Consortium>;
  credentialOptions: Array<{ name?: string; value: string }> = [
    { value: 'userAuth' },
    { value: 'certificate' },
    { value: 'configFile' },
  ];

  constructor(
    private consortiumService: ConsortiumService,
    private changeDetectorRef: ChangeDetectorRef,
    private fb: FormBuilder,
    private translate: TranslateService
  ) {

    this.gridOptions.getData = () => {
      return this.consortiumService.getFakeData();
      // return this.consortiumService.getList(params);
    };

    translate.get('consortium.grid')
      .subscribe(grid => this.handleGrid(grid));
  }

  ngOnInit() {
  }

  selectedRowChange(rows: Array<Consortium>): void {
    this.selectedRows = rows;
  }

  openAddConsortium(): void {
    this.openModal('add');
  }

  addConsortium(): void {
    const formModel = this.addConsortiumForm.value;

    const consortium: Consortium = {
      name: formModel.name,
      members: formModel.members,
    };

    this.openModalForm = false;

    this.consortiumService.create(consortium)
      .subscribe(response => this.handleAdd(response));
  }

  confirmDeleteConsortium(): void {
    this.openModal('delete');
  }

  deleteConsortium(): void {
    this.selectedRows.forEach(consortium => {
      this.consortiumService.delete(consortium.id)
        .subscribe(response => this.handleDeletion(response));
    });
  }

  onConfigFileChange(event) {
    if (event.target.files.length === 0) {
      this.addConsortiumForm.patchValue({
        configFile: null
      });
      return;
    }

    const reader = new FileReader();
    reader.onload = () => {
      this.addConsortiumForm.patchValue({
        configFile: reader.result
      });
      this.changeDetectorRef.markForCheck();
    };
    reader.readAsText(event.target.files[0]);
  }

  private createAddConsortiumForm() {
    this.addConsortiumForm = this.fb.group({
      name: ['', Validators.required],
      members: [[], Validators.required]
    });
  }

  private openModal(type: string): void {
    this.formType = type;

    switch (type) {
      case 'add':
        this.createAddConsortiumForm();
        this.modalSize = 'md';
        this.translate.get('consortium.addConsortiumForm.title')
          .subscribe(title => this.modalTitle = title);

        break;

      case 'delete':
        this.modalSize = 'sm';
        this.translate.get('consortium.deleteConsortiumForm.title')
          .subscribe(title => this.modalTitle = title);
        break;
    }

    this.openModalForm = true;
  }

  private handleAdd(response): void {
    console.log('add response', response);
    this.openModalForm = false;
    this.grid.addRow(response);
  }

  private handleDeletion(response): void {
    console.log('deletion response', response);
    this.openModalForm = false;
    this.grid.reload();
  }

  private handleGrid(grid: any): void {
    this.gridOptions.paginationTitle = grid.pagination.title;
    this.gridOptions.columns = [{
      id: 'name',
      name: grid.columns.name.title,
      type: 'string'
    }, {
      id: 'members',
      name: grid.columns.members.title,
      type: 'info',
      renderCell: {
        main: (row: Consortium) => {
          return row.members.length;
        },
        info: (row: Consortium) => {
          return `<h3>Members</h3>
            <ol class="list">
              ${row.members.join(',').split(',').map((members) => `
              <li>${members}</li>`).join('')}
            </ol>`;
        }
      }
    }, {
      id: 'createdOn',
      name: grid.columns.created.title,
      type: 'date'
    }];
  }
}
