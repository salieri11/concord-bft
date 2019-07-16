/*
 * Copyright 2019 VMware, all rights reserved.
 */

declare var window: any;

export class DynamicEnvironment {
    public get cspEnv() {
        return window.config.cspEnv;
    }
}
