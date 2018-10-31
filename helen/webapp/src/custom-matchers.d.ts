/*
 * Copyright 2018 VMware, all rights reserved.
 */

declare module jasmine {
    interface Matchers<T> {
        toHaveRendered (util: any): boolean;
    }
}
