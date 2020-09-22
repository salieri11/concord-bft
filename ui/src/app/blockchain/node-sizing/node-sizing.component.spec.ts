/*
 * Copyright 2018-2020 VMware, all rights reserved.
 */

import { testFor, beforeTesting, prepareEach } from '../../../test.helper.spec';

import { NodeSizingComponent } from './node-sizing.component';

describe('NodeSizingComponent', () => {

  const test = testFor(NodeSizingComponent).expedite({
    imports: [], provides: [], declarations: [NodeSizingComponent],
  }, beforeTesting(() => { }), prepareEach(() => { }));

  it('should create', () => {
    expect(test.component).toBeTruthy();
    expect(test.component.form.value).toEqual({
      committerSizing: { no_of_cpus: 1, memory_in_gigs: 1, storage_in_gigs: 1 },
      clientSizing: { no_of_cpus: 1, memory_in_gigs: 1, storage_in_gigs: 1 }
    });
    expect(test.component.hasClients).toBe(true);
  });

  it('should toggle between custom and templates', () => {
    const comp = test.component;
    spyOn(comp.isValid, 'emit');
    comp.selectSize('Small');
    expect(comp.selectedSizing).toBe('Small');

    comp.toggleTemplatesCustom();
    expect(comp.showTemplates).toBe(false);
    expect(comp.selectedSizing).toBe(undefined);
    expect(comp.isValid.emit).toHaveBeenCalledWith(false);
  });

  it('should select size and emit the size config', () => {
    const comp = test.component;
    spyOn(comp.sizeChange, 'emit');
    spyOn(comp.isValid, 'emit');
    comp.toggleTemplatesCustom();

    comp.selectSize('Small');
    expect(comp.selectedSizing).toBe('Small');

    expect(comp.sizeChange.emit).toHaveBeenCalledWith({
      committerSizing: {
        type: 'committer',
        no_of_cpus: '4',
        storage_in_gigs: '1024',
        memory_in_gigs: '32'
      },
      clientSizing: {
        type: 'client',
        no_of_cpus: '4',
        storage_in_gigs: '1024',
        memory_in_gigs: '32'
      }
    });
    expect(comp.isValid.emit).toHaveBeenCalledWith(true);
  });

  it('should fill out bad values in the form', () => {
    const comp = test.component;
    comp.toggleTemplatesCustom();
    comp.form.controls['committerSizing']['controls']['no_of_cpus'].setValue(5000);
    expect(comp.form.controls['committerSizing']['controls']['no_of_cpus'].valid).toBe(false);
    comp.form.controls['committerSizing']['controls']['no_of_cpus'].setValue(4);
    expect(comp.form.controls['committerSizing']['controls']['no_of_cpus'].valid).toBe(true);
  });
});


