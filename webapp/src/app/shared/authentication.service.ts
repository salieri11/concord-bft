/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { BehaviorSubject, Observable } from 'rxjs';
import { map } from 'rxjs/operators';

import { Personas, PersonaService } from './persona.service';
import { User } from '../users/shared/user.model';

@Injectable()
export class AuthenticationService {
  private userSubject: BehaviorSubject<User>;
  readonly user: Observable<User>;
  agreement: any = {accepted: false};

  constructor(private personaService: PersonaService, private http: HttpClient) {
    this.userSubject = new BehaviorSubject<User>({
      email: localStorage['helen.email'],
      persona: localStorage['helen.persona']
    });
    this.user = this.userSubject.asObservable();
    this.personaService.currentPersona = localStorage['helen.persona'];
  }

  isAuthenticated() {
    return localStorage['helen.email'] !== undefined;
  }

  handleLogIn(email: string, password: string, persona: Personas) {
    password = '';
    localStorage.setItem('helen.email', email);
    localStorage.setItem('helen.password', password);
    this.personaService.currentPersona = persona;
    this.userSubject.next({
      email: email,
      persona: persona
    });
  }

  logIn(email: string, password: string, persona: Personas) {
    const url = 'api/login';
    return this.http.post<{email: string, password: string}>(url, {email: email, password: password}).pipe(
      map((response) => {
        this.handleLogIn(email, password, persona);
        return response;
      })
    );
  }

  logOut() {
    localStorage.removeItem('helen.email');
    localStorage.removeItem('helen.persona');
    this.personaService.currentPersona = undefined;
    this.userSubject.next({email: localStorage['helen.email'], persona: localStorage['helen.persona']});
  }

  checkForLegalAgreements(): Observable<any> {
    return this.http.get('/api/agreements/1').pipe(
        map((response) => {
          this.agreement = response;

          return response;
        }),
      );
  }

  acceptLegalAgreement(params: any): Observable<any> {
    return this.http.patch<any>('api/agreements/1', params);
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
