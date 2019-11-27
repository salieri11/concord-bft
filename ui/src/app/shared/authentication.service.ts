/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { Router } from '@angular/router';
import { HttpClient } from '@angular/common/http';
import { BehaviorSubject, Observable } from 'rxjs';
import { map, flatMap } from 'rxjs/operators';

import { Personas, PersonaService } from './persona.service';
import { User, UserAuthResponse } from '../users/shared/user.model';
import { UsersService } from '../users/shared/users.service';
import { OrgService } from '../orgs/shared/org.service';

@Injectable()
export class AuthenticationService {
  private userSubject: BehaviorSubject<User>;
  readonly user: Observable<User>;
  agreement: boolean = false;
  redirectUrl: string;
  accessToken: string;
  logoutPath: string = '/api/oauth/logout';
  parsedToken: any;
  orgProps: any;

  constructor(
    private personaService: PersonaService,
    private usersService: UsersService,
    private orgService: OrgService,
    private http: HttpClient,
    private router: Router,
  ) {
    this.userSubject = new BehaviorSubject<User>({
      email: localStorage['helen.email'],
      consortium_id: localStorage['helen.consortium_id'],
      persona: localStorage['helen.persona'],
      user_id: localStorage['helen.user_id'],
      wallet_address: localStorage['helen.wallet_address']
    });
    this.user = this.userSubject.asObservable();
  }

  isAuthenticated() {
    return localStorage['helen.user_id'] !== undefined;
  }

  get currentUser() {
    return {
      email: localStorage['helen.email'],
      consortium_id: localStorage['helen.consortium_id'],
      persona: localStorage['helen.persona'],
      user_id: localStorage['helen.user_id'],
      wallet_address: localStorage['helen.wallet_address']
    };
  }

  getAccessToken(): Observable<UserAuthResponse> {
    const url = 'api/oauth/token';
    return this.http.get<UserAuthResponse>(url).pipe(
      map(response => {
        this.accessToken = response.auth_token;
        this.parsedToken = this.parseJwt(this.accessToken);
        this.setPersona(this.parsedToken.perms);

        return response;
      }),
      flatMap(response => this.resolveOrgProperties(response))
    );
  }

  refreshToken(): Observable<any> {
    const url = 'api/auth/token';
    return this.http.post<{ refresh_token: string }>(url, {
      refresh_token: localStorage.getItem('jwtRefreshToken')
    }).pipe(
      map(response => {
        this.setToken(response);
        return response;
      })
    );
  }

  logIn(email: string, password: string, persona: Personas): Observable<any> {
    const url = 'api/auth/login';
    return this.http.post<{ email: string, password: string }>(url, {
      email: email,
      password: password
    }).pipe(
      map(response => {
        this.handleLogin(response, persona);
        return response;
      })
    );
  }

  changePassword(email: string, newPassword: string) {
    const url = 'api/auth/change-password';
    return this.http.post<{ email: string, password: string }>(
      url, {
        email: email,
        password: newPassword
      }
    );
  }

  logOut() {
    localStorage.removeItem('helen.email');
    localStorage.removeItem('jwtToken');
    localStorage.removeItem('jwtRefreshToken');
    localStorage.removeItem('jwtTokenExpiresAt');
    localStorage.removeItem('jwtRefreshTokenExpiresAt');
    localStorage.removeItem('helen.consortium_id');
    localStorage.removeItem('helen.persona');
    localStorage.removeItem('helen.user_id');
    localStorage.removeItem('helen.wallet_address');
    this.personaService.currentPersonas = undefined;
    this.userSubject.next({
      email: localStorage['helen.email'],
      consortium_id: localStorage['helen.consortium_id'],
      persona: localStorage['helen.persona'],
      user_id: localStorage['helen.user_id'],
      wallet_address: localStorage['helen.wallet_address']
    });
    this.setRedirectPath();
    this.router.navigate(['auth/login']);
  }

  setRedirectPath() {
    const redirect = this.router.url;
    const disallowedRedirects = ['/auth/login'];

    if (disallowedRedirects.indexOf(redirect) === -1) {
      this.redirectUrl = redirect;
    }
  }


  checkForLegalAgreements(): Observable<boolean> {
    return this.http.get<Array<any>>('api/organizations/agreements').pipe(
      map((response) => {
        this.agreement = response.length !== 0;
        return this.agreement;
      }),
    );
  }

  getLegalAgreement(): Observable<any> {
    return this.http.get('static/agreements/tos.txt', {responseType: 'text'});
  }

  acceptLegalAgreement(params: any): Observable<any> {
    return this.http.post<any>('api/organizations/agreements', params);
  }

  handleLogin(response: User, persona: Personas) {
    const walletAddress = response.wallet_address ? `0x${response.wallet_address}` : '';
    localStorage.setItem('helen.user_id', response.user_id);
    localStorage.setItem('helen.email', response.email);
    localStorage.setItem('helen.wallet_address', walletAddress);
    this.setToken(response);
    this.personaService.currentPersonas.push(persona);
    this.usersService.getList().subscribe((users) => {
      const consortiumId = users[0].consortium.consortium_id;
      localStorage.setItem('helen.consortium_id', consortiumId);
      this.userSubject.next({
        email: response.email,
        consortium_id: consortiumId,
        persona: persona,
        user_id: response.user_id,
        wallet_address: walletAddress
      });
    });
  }

  private setPersona(perms: string[]) {
    perms.forEach(perm => {
      if (perm.startsWith('external')) {
        const permList = perm.split('/');

        switch (permList[2]) {
          case 'vmbc-system:admin':
            this.personaService.currentPersonas.push(Personas.SystemsAdmin);
            break;
          case 'vmbc-system:infra':
            this.personaService.currentPersonas.push(Personas.SystemsInfra);
            break;
          case 'vmbc-org:user':
            this.personaService.currentPersonas.push(Personas.OrgUser);
            break;
          case 'vmbc-org:dev':
            this.personaService.currentPersonas.push(Personas.OrgDeveloper);
            break;
          case 'vmbc-org:admin':
            this.personaService.currentPersonas.push(Personas.OrgAdmin);
            break;
          case 'vmbc-consortium:admin':
            this.personaService.currentPersonas.push(Personas.ConsortiumAdmin);
            break;
          case 'vmbc-consortium:operator':
            this.personaService.currentPersonas.push(Personas.ConsortiumOperator);
            break;
          case 'vmbc-consortium:participant':
            this.personaService.currentPersonas.push(Personas.ConsortiumParticipant);
            break;

          default:
            break;
        }
      }
    });
  }
  private setToken(response): void {
    const expiresAt = Date.now() + response.token_expires;
    const refreshExpiresAt = Date.now() + (response.token_expires * 2);

    localStorage.setItem('jwtToken', response.token);
    localStorage.setItem('jwtRefreshToken', response.refresh_token);
    localStorage.setItem('jwtTokenExpiresAt', expiresAt.toString());
    localStorage.setItem('jwtRefreshTokenExpiresAt', refreshExpiresAt.toString());
  }

  private parseJwt(token: string): any {
    const base64Url = token.split('.')[1];
    const base64 = base64Url.replace(/-/g, '+').replace(/_/g, '/');
    const jsonPayload = decodeURIComponent(atob(base64).split('').map(function (c) {
      return '%' + ('00' + c.charCodeAt(0).toString(16)).slice(-2);
    }).join(''));

    return JSON.parse(jsonPayload);
  }

  private resolveOrgProperties(response): Observable<any> {
    const orgId = this.parsedToken.context_name;

    return this.orgService.getDetail(orgId).pipe(
      map(res => {
        if (res.organization_properties === null) {
          res.organization_properties = {max_chains: 1};
        }
        res.organization_properties.max_chains = Number(res.organization_properties.max_chains);
        this.orgProps = res.organization_properties;

        return response;
      })
    );
  }

  // TODO: Use country list from CSP VIP
  getCountryList(): Array<string> {
    return [
      `AUSTRALIA`,
      `CANADA`,
      `FRANCE`,
      `UNITED STATES`,
      `---------------`,
      `AFGHANISTAN`,
      `ALBANIA`,
      `ALGERIA`,
      `AMERICAN SAMOA`,
      `ANDORRA`,
      `ANGOLA`,
      `ANGUILLA`,
      `ANTARCTICA`,
      `ANTIGUA AND BARBUDA`,
      `ARGENTINA`,
      `ARUBA`,
      `AUSTRIA`,
      `BAHAMAS`,
      `BAHRAIN`,
      `BANGLADESH`,
      `BARBADOS`,
      `BELARUS`,
      `BELGIUM`,
      `BELIZE`,
      `BENIN`,
      `BERMUDA`,
      `BHUTAN`,
      `BOLIVIA`,
      `BONAIRE`,
      `BOSNIA AND HERZEGOVINA`,
      `BOTSWANA`,
      `BRAZIL`,
      `BRITISH INDIAN OCEAN TERRITORY`,
      `BRUNEI DARUSSALAM`,
      `BULGARIA`,
      `BURKINA FASO`,
      `BURUNDI`,
      `CAMBODIA`,
      `CAMEROON`,
      `CAPE VERDE`,
      `CAYMAN ISLANDS`,
      `CENTRAL AFRICAN REPUBLIC`,
      `CHAD`,
      `CHILE`,
      `CHINA`,
      `CHRISTMAS ISLAND`,
      `COCOS (KEELING) ISLANDS`,
      `COLOMBIA`,
      `COMOROS`,
      `COOK ISLANDS`,
      `COSTA RICA`,
      `CROATIA`,
      `CURACAO`,
      `CYPRUS`,
      `CZECH REPUBLIC`,
      `CÔTE D'IVOIRE`,
      `DEMOCRATIC REPUBLIC OF THE CONGO`,
      `DENMARK`,
      `DJIBOUTI`,
      `DOMINICA`,
      `DOMINICAN REPUBLIC`,
      `ECUADOR`,
      `EGYPT`,
      `EL SALVADOR`,
      `EQUATORIAL GUINEA`,
      `ERITREA`,
      `ESTONIA`,
      `ETHIOPIA`,
      `FALKLAND ISLANDS (MALVINAS)`,
      `FAROE ISLANDS`,
      `FIJI`,
      `FINLAND`,
      `FRENCH GUIANA`,
      `FRENCH POLYNESIA`,
      `FRENCH SOUTHERN TERRITORIES`,
      `GABON`,
      `GAMBIA`,
      `GEORGIA`,
      `GERMANY`,
      `GHANA`,
      `GIBRALTAR`,
      `GREECE`,
      `GREENLAND`,
      `GRENADA`,
      `GUADELOUPE`,
      `GUAM`,
      `GUATEMALA`,
      `GUERNSEY`,
      `GUINEA`,
      `GUINEA-BISSAU`,
      `GUYANA`,
      `HAITI`,
      `HEARD ISLAND AND MCDONALD ISLANDS`,
      `HOLY SEE (VATICAN CITY STATE)`,
      `HONDURAS`,
      `HONG KONG`,
      `HUNGARY`,
      `ICELAND`,
      `INDIA`,
      `INDONESIA`,
      `IRAQ`,
      `IRELAND`,
      `ISLE OF MAN`,
      `ISRAEL`,
      `ITALY`,
      `JAMAICA`,
      `JAPAN`,
      `JERSEY`,
      `JORDAN`,
      `KAZAKHSTAN`,
      `KENYA`,
      `KIRIBATI`,
      `KOREA, REPUBLIC OF`,
      `KUWAIT`,
      `KYRGYZSTAN`,
      `LAO PEOPLE\\'S DEMOCRATIC REPUBLIC`,
      `LATVIA`,
      `LEBANON`,
      `LESOTHO`,
      `LIBERIA`,
      `LIBYAN ARAB JAMAHIRIYA`,
      `LIECHTENSTEIN`,
      `LITHUANIA`,
      `LUXEMBOURG`,
      `MACAO`,
      `MACEDONIA, THE FORMER YUGOSLAV REPUBLIC OF`,
      `MADAGASCAR`,
      `MALAWI`,
      `MALAYSIA`,
      `MALDIVES`,
      `MALI`,
      `MALTA`,
      `MARSHALL ISLANDS`,
      `MARTINIQUE`,
      `MAURITANIA`,
      `MAURITIUS`,
      `MAYOTTE`,
      `MEXICO`,
      `MICRONESIA, FEDERATED STATES OF`,
      `MOLDOVA, REPUBLIC OF`,
      `MONACO`,
      `MONGOLIA`,
      `MONTENEGRO`,
      `MONTSERRAT`,
      `MOROCCO`,
      `MOZAMBIQUE`,
      `MYANMAR`,
      `NAMIBIA`,
      `NAURU`,
      `NEPAL`,
      `NETHERLANDS`,
      `NETHERLANDS ANTILLES`,
      `NEW CALEDONIA`,
      `NEW ZEALAND`,
      `NICARAGUA`,
      `NIGER`,
      `NIGERIA`,
      `NIUE`,
      `NORFOLK ISLAND`,
      `NORTHERN MARIANA ISLANDS`,
      `NORWAY`,
      `OMAN`,
      `PAKISTAN`,
      `PALAU`,
      `PANAMA`,
      `PAPUA NEW GUINEA`,
      `PARAGUAY`,
      `PERU`,
      `PHILIPPINES`,
      `PITCAIRN`,
      `POLAND`,
      `PORTUGAL`,
      `PUERTO RICO`,
      `QATAR`,
      `ROMANIA`,
      `RUSSIAN FEDERATION`,
      `RWANDA`,
      `RÉUNION`,
      `SABA`,
      `SAINT BARTHÉLEMY`,
      `SAINT EUSTATIUS`,
      `SAINT HELENA`,
      `SAINT KITTS AND NEVIS`,
      `SAINT LUCIA`,
      `SAINT MARTIN`,
      `SAINT PIERRE AND MIQUELON`,
      `SAINT VINCENT AND THE GRENADINES`,
      `SAMOA`,
      `SAN MARINO`,
      `SAO TOME AND PRINCIPE`,
      `SAUDI ARABIA`,
      `SCOTLAND`,
      `SENEGAL`,
      `SERBIA`,
      `SEYCHELLES`,
      `SIERRA LEONE`,
      `SINGAPORE`,
      `SLOVAKIA`,
      `SLOVENIA`,
      `SOLOMON ISLANDS`,
      `SOMALIA`,
      `SOUTH AFRICA`,
      `SOUTH GEORGIA AND THE SOUTH SANDWICH ISLANDS`,
      `SPAIN`,
      `SRI LANKA`,
      `SURINAME`,
      `SVALBARD AND JAN MAYEN`,
      `SWAZILAND`,
      `SWEDEN`,
      `SWITZERLAND`,
      `TAIWAN`,
      `TAJIKISTAN`,
      `TANZANIA, UNITED REPUBLIC OF`,
      `THAILAND`,
      `TIMOR-LESTE`,
      `TOGO`,
      `TOKELAU`,
      `TONGA`,
      `TRINIDAD AND TOBAGO`,
      `TUNISIA`,
      `TURKEY`,
      `TURKMENISTAN`,
      `TURKS AND CAICOS ISLANDS`,
      `TUVALU`,
      `UGANDA`,
      `UKRAINE`,
      `UNITED ARAB EMIRATES`,
      `UNITED KINGDOM`,
      `UNITED STATES MINOR OUTLYING ISLANDS`,
      `URUGUAY`,
      `UZBEKISTAN`,
      `VANUATU`,
      `VENEZUELA, BOLIVARIAN REPUBLIC OF`,
      `VIET NAM`,
      `VIRGIN ISLANDS, BRITISH`,
      `VIRGIN ISLANDS, U.S.`,
      `WALLIS AND FUTUNA`,
      `YEMEN`,
      `ZAMBIA`,
      `ZIMBABWE`,
      `ÅLAND ISLANDS`,
    ];

  }

}
