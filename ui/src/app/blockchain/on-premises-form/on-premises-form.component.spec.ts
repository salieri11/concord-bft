import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { OnPremisesFormComponent } from './on-premises-form.component';

describe('OnPremisesFormComponent', () => {
  let component: OnPremisesFormComponent;
  let fixture: ComponentFixture<OnPremisesFormComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ OnPremisesFormComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(OnPremisesFormComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
