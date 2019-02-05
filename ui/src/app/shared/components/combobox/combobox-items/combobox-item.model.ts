/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

export class VmwComboboxItem {
  public displayValue: string;
  public value: any;
  public iconShape?: string;

  constructor(displayValue: string, value: any, iconShape?: string) {
    this.displayValue = displayValue;
    this.value = value;
    this.iconShape = iconShape;
  }
}
