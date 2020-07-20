/*
 * Copyright 2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { CspApiService, CspTokenService } from '@vmw/csp-ngx-components';

import { MockSharedModule } from '../../shared/shared.module';
import { ForbiddenComponent } from './forbidden.component';
import { AppHeaderComponent } from './../../shared/components/app-header/app-header.component';
import { VmwThemeSwitchButtonComponent } from '../../shared/components/theme-switch-button/theme-switch-button.component';
import { AuthenticationService } from './../../shared/authentication.service';
import { CspNoServiceAccessPageMode } from '@vmw/csp-ngx-components';
import { VersionComponent } from '../../shared/components/version/version.component';
import { VmwClarityThemeService } from '../../shared/theme.provider';
import { TourService } from '../../shared/tour.service';
import { CanViewDirective } from '../../shared/directives/can-view.directive';

describe('ForbiddenComponent', () => {
  let component: ForbiddenComponent;
  let fixture: ComponentFixture<ForbiddenComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        MockSharedModule,
        RouterTestingModule
      ],
      declarations: [
        ForbiddenComponent,
        AppHeaderComponent,
        VmwThemeSwitchButtonComponent,
        VersionComponent,
        CanViewDirective
      ],
      providers: [
        {
          provide: AuthenticationService,
          useValue: {
            parsedToken: { context_name: 'hello' },
            // tslint:disable-next-line
            accessToken: 'eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6InNpZ25pbmdfMiJ9.eyJzdWIiOiJ2bXdhcmUuY29tOjY0ZTc1MGM2LWMxMTYtNGRjMy1iN2VjLWU3MjI0NmFiM2YxOSIsImF6cCI6IlBraWNRdktRSFBldEpidWJrSnhRYUx2anVjYWs4ODVsS1dyIiwiZG9tYWluIjoidm13YXJlLmNvbSIsImNvbnRleHQiOiIxZGM0N2U3ZS01YWM4LTRiZWEtYmQ4ZC1hOWEyYjc4OWM5ZGQiLCJpc3MiOiJodHRwczovL2dhei1wcmV2aWV3LmNzcC12aWRtLXByb2QuY29tIiwicGVybXMiOlsiY3NwOm9yZ19vd25lciIsImV4dGVybmFsLzJhNjA0ZjY4LWM3NDktNDFkYS04ODUyLTkzOGZlZmEyZjkyMC92bWJjLWNvbnNvcnRpdW06b3BlcmF0b3IiLCJleHRlcm5hbC8yYTYwNGY2OC1jNzQ5LTQxZGEtODg1Mi05MzhmZWZhMmY5MjAvdm1iYy1zeXN0ZW06YWRtaW4iLCJjc3A6c2VydmljZV9vd25lciIsImV4dGVybmFsLzJhNjA0ZjY4LWM3NDktNDFkYS04ODUyLTkzOGZlZmEyZjkyMC92bWJjLW9yZzpkZXYiLCJleHRlcm5hbC8yYTYwNGY2OC1jNzQ5LTQxZGEtODg1Mi05MzhmZWZhMmY5MjAvdm1iYy1vcmc6dXNlciIsImV4dGVybmFsLzJhNjA0ZjY4LWM3NDktNDFkYS04ODUyLTkzOGZlZmEyZjkyMC92bWJjLWNvbnNvcnRpdW06cGFydGljaXBhbnQiLCJleHRlcm5hbC8yYTYwNGY2OC1jNzQ5LTQxZGEtODg1Mi05MzhmZWZhMmY5MjAvdm1iYy1vcmc6YWRtaW4iLCJleHRlcm5hbC8yYTYwNGY2OC1jNzQ5LTQxZGEtODg1Mi05MzhmZWZhMmY5MjAvdm1iYy1jb25zb3J0aXVtOmFkbWluIl0sImNvbnRleHRfbmFtZSI6ImFiMjliNGE4LTQwYzgtNDE3My1iYTc0LTE1ZTc2ZjhmMzVlMCIsImV4cCI6MTU2NDU5ODE3NCwiaWF0IjoxNTY0NTk2Mzc0LCJqdGkiOiIxY2RkY2Q3Yi0zY2EyLTRhZTgtODk1My00OTVlMGE5NDlhYzciLCJhY2N0IjoibWhhcnJpc29uQHZtd2FyZS5jb20iLCJ1c2VybmFtZSI6Im1oYXJyaXNvbiJ9.Z1eFFX6KVc-ioJBGInGZEPRAaF-7r6k2CqfzPhnRdT2SldG4xyX80Zc31DoFYt4QwlhXp7DFwuZdHaHd3hu4HOaOekVnRloAtaejQTEoPDXiMQ0fLpMCy8rJqdH6gCKNVb5lzgZfeusJwvtA5ERn-IaHjVjvghCYejnASXWlSS7U0mkypNnd4tlKGC8vJzQ7OZRz-jOX15eehdUu5KD_rs1gmTJMZAI4vBQAe-bP0PBzjB6kad9si25R3kju6B3vUjVOiBq0LB4HMdrokolIHTW8zfXtGvxtM9jhee2D6jeL0df0FEhCyPfdQ-7Yt857MRdK7GKTWN9Xc1OHD9okJw',
            user: {
              subscribe: (fn: (value) => void) => fn(
                'test'
              ),
            },
          }
        },
        VmwClarityThemeService,
        TourService,
        {
          provide: CspApiService,
          useValue: {
            // tslint:disable-next-line
            token: 'eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6InNpZ25pbmdfMiJ9.eyJzdWIiOiJ2bXdhcmUuY29tOjY0ZTc1MGM2LWMxMTYtNGRjMy1iN2VjLWU3MjI0NmFiM2YxOSIsImF6cCI6IlBraWNRdktRSFBldEpidWJrSnhRYUx2anVjYWs4ODVsS1dyIiwiZG9tYWluIjoidm13YXJlLmNvbSIsImNvbnRleHQiOiIxZGM0N2U3ZS01YWM4LTRiZWEtYmQ4ZC1hOWEyYjc4OWM5ZGQiLCJpc3MiOiJodHRwczovL2dhei1wcmV2aWV3LmNzcC12aWRtLXByb2QuY29tIiwicGVybXMiOlsiY3NwOm9yZ19vd25lciIsImV4dGVybmFsLzJhNjA0ZjY4LWM3NDktNDFkYS04ODUyLTkzOGZlZmEyZjkyMC92bWJjLWNvbnNvcnRpdW06b3BlcmF0b3IiLCJleHRlcm5hbC8yYTYwNGY2OC1jNzQ5LTQxZGEtODg1Mi05MzhmZWZhMmY5MjAvdm1iYy1zeXN0ZW06YWRtaW4iLCJjc3A6c2VydmljZV9vd25lciIsImV4dGVybmFsLzJhNjA0ZjY4LWM3NDktNDFkYS04ODUyLTkzOGZlZmEyZjkyMC92bWJjLW9yZzpkZXYiLCJleHRlcm5hbC8yYTYwNGY2OC1jNzQ5LTQxZGEtODg1Mi05MzhmZWZhMmY5MjAvdm1iYy1vcmc6dXNlciIsImV4dGVybmFsLzJhNjA0ZjY4LWM3NDktNDFkYS04ODUyLTkzOGZlZmEyZjkyMC92bWJjLWNvbnNvcnRpdW06cGFydGljaXBhbnQiLCJleHRlcm5hbC8yYTYwNGY2OC1jNzQ5LTQxZGEtODg1Mi05MzhmZWZhMmY5MjAvdm1iYy1vcmc6YWRtaW4iLCJleHRlcm5hbC8yYTYwNGY2OC1jNzQ5LTQxZGEtODg1Mi05MzhmZWZhMmY5MjAvdm1iYy1jb25zb3J0aXVtOmFkbWluIl0sImNvbnRleHRfbmFtZSI6ImFiMjliNGE4LTQwYzgtNDE3My1iYTc0LTE1ZTc2ZjhmMzVlMCIsImV4cCI6MTU2NDU5ODE3NCwiaWF0IjoxNTY0NTk2Mzc0LCJqdGkiOiIxY2RkY2Q3Yi0zY2EyLTRhZTgtODk1My00OTVlMGE5NDlhYzciLCJhY2N0IjoibWhhcnJpc29uQHZtd2FyZS5jb20iLCJ1c2VybmFtZSI6Im1oYXJyaXNvbiJ9.Z1eFFX6KVc-ioJBGInGZEPRAaF-7r6k2CqfzPhnRdT2SldG4xyX80Zc31DoFYt4QwlhXp7DFwuZdHaHd3hu4HOaOekVnRloAtaejQTEoPDXiMQ0fLpMCy8rJqdH6gCKNVb5lzgZfeusJwvtA5ERn-IaHjVjvghCYejnASXWlSS7U0mkypNnd4tlKGC8vJzQ7OZRz-jOX15eehdUu5KD_rs1gmTJMZAI4vBQAe-bP0PBzjB6kad9si25R3kju6B3vUjVOiBq0LB4HMdrokolIHTW8zfXtGvxtM9jhee2D6jeL0df0FEhCyPfdQ-7Yt857MRdK7GKTWN9Xc1OHD9okJw',
            getService: () => {
              return true;
            },
            fetchUserOrgs: () => {
              return true;
            },
            getOrgDefaultPaymentMethod: () => {
              return true;
            },
            fetchServiceFamiliesForParentService: () => {
              return true;
            },
            getRedirectLink: () => {
              return 'string';
            }
          }
        }, {
          provide: CspTokenService,
          useValue: {
            authToken: () => {
              // tslint:disable-next-line
              return 'eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6InNpZ25pbmdfMiJ9.eyJzdWIiOiJ2bXdhcmUuY29tOjY0ZTc1MGM2LWMxMTYtNGRjMy1iN2VjLWU3MjI0NmFiM2YxOSIsImF6cCI6IlBraWNRdktRSFBldEpidWJrSnhRYUx2anVjYWs4ODVsS1dyIiwiZG9tYWluIjoidm13YXJlLmNvbSIsImNvbnRleHQiOiIxZGM0N2U3ZS01YWM4LTRiZWEtYmQ4ZC1hOWEyYjc4OWM5ZGQiLCJpc3MiOiJodHRwczovL2dhei1wcmV2aWV3LmNzcC12aWRtLXByb2QuY29tIiwicGVybXMiOlsiY3NwOm9yZ19vd25lciIsImV4dGVybmFsLzJhNjA0ZjY4LWM3NDktNDFkYS04ODUyLTkzOGZlZmEyZjkyMC92bWJjLWNvbnNvcnRpdW06b3BlcmF0b3IiLCJleHRlcm5hbC8yYTYwNGY2OC1jNzQ5LTQxZGEtODg1Mi05MzhmZWZhMmY5MjAvdm1iYy1zeXN0ZW06YWRtaW4iLCJjc3A6c2VydmljZV9vd25lciIsImV4dGVybmFsLzJhNjA0ZjY4LWM3NDktNDFkYS04ODUyLTkzOGZlZmEyZjkyMC92bWJjLW9yZzpkZXYiLCJleHRlcm5hbC8yYTYwNGY2OC1jNzQ5LTQxZGEtODg1Mi05MzhmZWZhMmY5MjAvdm1iYy1vcmc6dXNlciIsImV4dGVybmFsLzJhNjA0ZjY4LWM3NDktNDFkYS04ODUyLTkzOGZlZmEyZjkyMC92bWJjLWNvbnNvcnRpdW06cGFydGljaXBhbnQiLCJleHRlcm5hbC8yYTYwNGY2OC1jNzQ5LTQxZGEtODg1Mi05MzhmZWZhMmY5MjAvdm1iYy1vcmc6YWRtaW4iLCJleHRlcm5hbC8yYTYwNGY2OC1jNzQ5LTQxZGEtODg1Mi05MzhmZWZhMmY5MjAvdm1iYy1jb25zb3J0aXVtOmFkbWluIl0sImNvbnRleHRfbmFtZSI6ImFiMjliNGE4LTQwYzgtNDE3My1iYTc0LTE1ZTc2ZjhmMzVlMCIsImV4cCI6MTU2NDU5ODE3NCwiaWF0IjoxNTY0NTk2Mzc0LCJqdGkiOiIxY2RkY2Q3Yi0zY2EyLTRhZTgtODk1My00OTVlMGE5NDlhYzciLCJhY2N0IjoibWhhcnJpc29uQHZtd2FyZS5jb20iLCJ1c2VybmFtZSI6Im1oYXJyaXNvbiJ9.Z1eFFX6KVc-ioJBGInGZEPRAaF-7r6k2CqfzPhnRdT2SldG4xyX80Zc31DoFYt4QwlhXp7DFwuZdHaHd3hu4HOaOekVnRloAtaejQTEoPDXiMQ0fLpMCy8rJqdH6gCKNVb5lzgZfeusJwvtA5ERn-IaHjVjvghCYejnASXWlSS7U0mkypNnd4tlKGC8vJzQ7OZRz-jOX15eehdUu5KD_rs1gmTJMZAI4vBQAe-bP0PBzjB6kad9si25R3kju6B3vUjVOiBq0LB4HMdrokolIHTW8zfXtGvxtM9jhee2D6jeL0df0FEhCyPfdQ-7Yt857MRdK7GKTWN9Xc1OHD9okJw';
            },
            isOrgOwner: () => {
              return true;
            }
          }
        }
      ]
    })
      .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ForbiddenComponent);
    component = fixture.componentInstance;
    component.mode = CspNoServiceAccessPageMode.NORMAL;

    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
