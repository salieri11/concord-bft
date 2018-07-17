import { Injectable } from '@angular/core';
import { BehaviorSubject } from "rxjs/internal/BehaviorSubject";
import { Observable } from "rxjs/internal/Observable";

@Injectable({
  providedIn: 'root'
})
export class TourService {
  private userProfileDropdownChangeSubject: BehaviorSubject<boolean> = new BehaviorSubject<boolean>(null);
  userProfileDropdownChanges$: Observable<boolean> = this.userProfileDropdownChangeSubject.asObservable();

  private userActionsDropdownChangeSubject: BehaviorSubject<boolean> = new BehaviorSubject<boolean>(null);
  userActionsDropdownChanges$: Observable<boolean> = this.userActionsDropdownChangeSubject.asObservable();

  private scrollSubject: BehaviorSubject<boolean> = new BehaviorSubject<boolean>(null);
  scrollSubjectChanges$: Observable<boolean> = this.scrollSubject.asObservable();

  isUserProfileMenuOpen: boolean = false;
  isUserActionsMenuOpen: boolean = false;

  constructor() { }

  toggleUserProfileMenu() {
    this.isUserProfileMenuOpen = !this.isUserProfileMenuOpen;
    this.userProfileDropdownChangeSubject.next(this.isUserProfileMenuOpen);
  }


  toggleUserActionsMenu() {
    this.isUserActionsMenuOpen = !this.isUserActionsMenuOpen;
    console.log(this.isUserActionsMenuOpen);
    this.userActionsDropdownChangeSubject.next(this.isUserActionsMenuOpen);
  }

  scrollToElement() {
    this.scrollSubject.next(true);
  }
}
