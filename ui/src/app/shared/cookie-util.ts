/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

export class VmwCookieUtil {
    static TEN_YEARS = 60 * 60 * 24 * 365 * 10;

    static getCookie(name: string) {
        const cookies = document.cookie.split(';');

        for (const cookie of cookies) {
            const bits = cookie.split('=');
            if (bits && bits[0].trim() === name) {
                return bits[1].trim();
            }
        }

        return null;
    }

    /*
     * Set a cookie with the provided metadata.
     *
     * If the requested domain is not available, cookie
     * domain will be set to location.hostname.
     */
    static setCookie(name: string, value: string,
            path: string = '/',
            maxAge: number = VmwCookieUtil.TEN_YEARS,
            domain: string = location.hostname) {
        if (location.hostname.indexOf(domain) === -1) {
            domain = location.hostname;
        }

        document.cookie = `${name}=${value};domain=${domain};max-age=${maxAge};path=${path};`;
    }
}
