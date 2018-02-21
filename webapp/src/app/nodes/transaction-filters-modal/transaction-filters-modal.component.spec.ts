import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { ClarityModule } from '@clr/angular';

import { TransactionFiltersModalComponent } from './transaction-filters-modal.component';

describe('TransactionFiltersModalComponent', () => {
  let component: TransactionFiltersModalComponent;
  let fixture: ComponentFixture<TransactionFiltersModalComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [ ClarityModule ],
      declarations: [ TransactionFiltersModalComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TransactionFiltersModalComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
