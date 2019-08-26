import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { OnPremisesModalComponent } from './on-premises-modal.component';

describe('OnPremisesModalComponent', () => {
  let component: OnPremisesModalComponent;
  let fixture: ComponentFixture<OnPremisesModalComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ OnPremisesModalComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(OnPremisesModalComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
