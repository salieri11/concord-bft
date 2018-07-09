import { AbstractControl, ValidatorFn } from "@angular/forms";
import { Subscription } from "rxjs/internal/Subscription";

export function matchPasswordValidator(passwordFormControlName: string): ValidatorFn {
  return (control: AbstractControl): any => {
    const otherPasswordFormControl: AbstractControl = control.root.get(passwordFormControlName);

    if (otherPasswordFormControl) {
      const subscription: Subscription = otherPasswordFormControl.valueChanges.subscribe(() => {
        control.updateValueAndValidity();
        subscription.unsubscribe();
      });
    }
    return (otherPasswordFormControl && control.value !== otherPasswordFormControl.value) ? { mismatch: true } : null;
  }
}
