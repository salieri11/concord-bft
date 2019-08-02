/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import {
  Component,
  OnInit,
  ViewChild,
  Output,
  EventEmitter,
  ElementRef
} from '@angular/core';
import { Router } from '@angular/router';
import { FormControl, FormGroup, Validators, ValidatorFn, ValidationErrors } from '@angular/forms';
import { ClrWizard, ClrWizardPage } from '@clr/angular';
import { PersonaService } from '../../persona.service';
import { BlockchainService } from '../../blockchain.service';
import { BlockchainRequestParams, Zone } from '../../blockchain.model';


const RegionCountValidator: ValidatorFn = (fg: FormGroup): ValidationErrors | null => {
  const nodes = fg['controls'].numberOfNodes.value;
  const zones = fg['controls'].zones;
  let count = 0;

  Object.keys(zones.value).forEach(key => {
    count = count + Number(zones.value[key]);
  });

  return nodes === count ? { 'countIsCorrect': true } : { 'countIsCorrect': false };
};


@Component({
  selector: 'concord-blockchain-wizard',
  templateUrl: './blockchain-wizard.component.html',
  styleUrls: ['./blockchain-wizard.component.scss']
})
export class BlockchainWizardComponent implements OnInit {
  @ViewChild('wizard') wizard: ClrWizard;
  @ViewChild('detailPage') blockConsortiumPage: ClrWizardPage;
  @ViewChild('usersPage') usersPage: ClrWizardPage;
  @ViewChild('consortiumInput') consortiumInput: ElementRef;
  @Output('setupComplete') setupComplete: EventEmitter<any> = new EventEmitter<any>();

  isOpen = false;
  form: FormGroup;
  nodeForm: FormGroup;
  userForm: FormGroup;

  personaOptions = PersonaService.getOptions();
  numbersOfNodes = [4, 7];
  fCountMapping = { '4': 1, '7': 2 };
  zones: Zone[] = [];

  constructor(
    private router: Router,
    private blockchainService: BlockchainService
  ) {
    this.zones = this.blockchainService.zones;
    this.form = this.initForm();


    this.nodeForm = new FormGroup({
      nodes: new FormGroup({
      }),
    });

    this.userForm = new FormGroup({
      email: new FormControl('', [Validators.required, Validators.email]),
      role: new FormControl('', Validators.required)
    });
  }

  ngOnInit() {
  }

  addUser() {
    const selectedUsers = this.form.get('users');
    selectedUsers.setValue(selectedUsers.value.concat([this.userForm.value]));
    this.userForm.reset();
  }

  resetFragment() {
    this.router.navigate([]);
  }

  deleteUser(index: number) {
    const selectedUsers = this.form.get('users');

    // @ts-ignore: no unused locals
    selectedUsers.setValue(selectedUsers.value.filter((item, i) => {
      return index !== i;
    }));
  }

  personaName(value): string {
    return PersonaService.getName(value);
  }

  open() {
    this.isOpen = true;
    if (this.form) {
      this.wizard.reset();
      this.form.reset();
    }

    setTimeout(() => {
      this.consortiumInput.nativeElement.focus();
    }, 500);
  }

  onSubmit() {
    const params = new BlockchainRequestParams();
    params.f_count = Number(this.fCountMapping[this.form.value.nodes.numberOfNodes.toString()]);
    params.consortium_name = this.form.value.details.consortium_name;

    // @ts-ignore
    const zones = this.form.controls['nodes'].controls['zones'].value;
    let zoneIds = [];
    // Create an array of zone ids for each deployed node instance.
    Object.keys(zones).forEach(zoneId => zoneIds = zoneIds.concat(Array(zones[zoneId]).fill(zoneId)));
    params.zone_ids = zoneIds;

    // this.blockchainService.notify.next({ message: 'deploying' });
    this.blockchainService.deploy(params).subscribe(response => {
      this.setupComplete.emit(response);
      // this.setupComplete.emit({task_id:'hello'});
      this.router.navigate(['/deploying', 'dashboard'], {
        queryParams: { task_id: response['task_id'] }
      });
    }, error => {
      this.setupComplete.emit(error);
    });
  }

  close() {
    this.isOpen = false;
  }

  distributeZones() {
    const zones = this.form.controls.nodes['controls'].zones;
    const nodes = this.form.controls.nodes['controls'].numberOfNodes;
    const regionKeys = Object.keys(zones.value);
    const ratio = regionKeys.length;
    const spread = [];
    // Clear out previous distribution
    zones.reset();

    // Create regional spread
    for (let index = 0; index < nodes.value; index++) {
      const idx = index % ratio;
      let val = spread[idx];

      if (val) {
        spread[idx] = ++val;
      } else {
        spread[idx] = 1;
      }
    }

    // Two loops so we don't patch the value multiple times on the same item,
    // because it fires off events each time we do that. This is more efficient.
    for (let index = 0; index < spread.length; index++) {
      const item = zones.controls[regionKeys[index]];
      item.patchValue(spread[index]);
    }
  }

  initForm(): FormGroup {
    return new FormGroup({
      details: new FormGroup({
        consortium_name: new FormControl('', Validators.required),
        consortium_desc: new FormControl('', Validators.required),
      }),
      nodes: new FormGroup({
        numberOfNodes: new FormControl('', Validators.required),
        zones: this.zoneGroup(),
      }, { validators: RegionCountValidator })
    });
  }
  private zoneGroup() {
    const group = {};

    this.zones.forEach(zone => {
      group[zone.id] = new FormControl('');
    });

    return new FormGroup(group);
  }

}
