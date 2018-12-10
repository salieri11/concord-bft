/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { Subscription } from 'rxjs';
import { ClrDropdown } from '@clr/angular';

import { AuthenticationService } from '../../authentication.service';
import { Personas, PersonaService } from '../../persona.service';
import { TourService } from '../../tour.service';

@Component({
  selector: 'concord-app-header',
  templateUrl: './app-header.component.html',
  styleUrls: ['./app-header.component.scss']
})
export class AppHeaderComponent implements OnDestroy, OnInit {
  @ViewChild('userProfileMenu') userProfileMenu: ClrDropdown;
  authenticationChange: Subscription;
  userProfileMenuToggleChanges: Subscription;
  personas = Personas;
  personaOptions = PersonaService.getOptions();
  authenticated = false;
  username: string;

  constructor(
    private authenticationService: AuthenticationService,
    private personaService: PersonaService,
    private tourService: TourService
  ) {
    this.authenticationChange = authenticationService.user.subscribe(user => {
      this.username = user.email;
      this.personaService.currentPersona = user.persona;
    });

    this.userProfileMenuToggleChanges = this.tourService.userProfileDropdownChanges$.subscribe((openMenu) => {
      setTimeout(() => {
        this.userProfileMenu.ifOpenService.open = openMenu;
      });
    });
  }

  ngOnInit() {
  }

  ngOnDestroy(): void {
    this.authenticationChange.unsubscribe();
  }

  onLogOut() {
    this.authenticationService.logOut();
  }

  onPersonaChange(persona: Personas) {
    localStorage.setItem('helen.persona', persona);
    this.personaService.currentPersona = persona;
    location.reload();
  }
}
