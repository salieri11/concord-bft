import { FormControl } from "@angular/forms";
import { isHexAddress, isHexadecimal, isInt, isUint } from "./custom-validators";

describe('CustomValidators', () => {

  describe('Hex address validation', () => {

    it('should not validate when address is not a valid hex address', () => {
      const control = new FormControl('', isHexAddress);
      control.setValue('test');
      expect(control.valid).toBeFalsy();
    });

    it('should validate if address is a valid hex address', () => {
      const control = new FormControl('', isHexAddress);
      control.setValue('0x262C0D7AB5FFD4EDE2199F6EA793F819E1ABB019');
      expect(control.valid).toBeTruthy();
    });

  });

  describe('Hexadecimal validation', () => {

    it('should not validate when value is not a valid hexadecimal value', () => {
      const control = new FormControl('', isHexadecimal);
      control.setValue('test');
      expect(control.valid).toBeFalsy();
    });

    it('should validate if value is a valid hexadecimal value', () => {
      const control = new FormControl('', isHexadecimal);
      control.setValue('0x217');
      expect(control.valid).toBeTruthy();
    });

  });

  describe('unsigned integer validation', () => {

    it('should not validate when value is not a valid unsigned integer', () => {
      const control = new FormControl('', isUint('uint256'));
      control.setValue('-0x80');
      expect(control.valid).toBeFalsy();
    });

    it('should validate if value is a valid unsigned integer', () => {
      const control = new FormControl('', isUint('uint256'));
      control.setValue('0xffff');
      expect(control.valid).toBeTruthy();
    });

    it('should validate as uint256 when no byte size is provided', () => {
      const control = new FormControl('', isUint('uint'));
      control.setValue('0x80');
      expect(control.valid).toBeTruthy();
    });

  });

  describe('signed integer validation', () => {

    it('should not validate when value is not a valid signed integer', () => {
      const control = new FormControl('', isInt('int8'));
      control.setValue('0x7ffff');
      expect(control.valid).toBeFalsy();
    });

    it('should validate if value is a valid signed integer', () => {
      const control = new FormControl('', isInt('int256'));
      control.setValue('0xffff');
      expect(control.valid).toBeTruthy();
    });

    it('should validate as int256 when no byte size is provided', () => {
      const control = new FormControl('', isUint('int'));
      control.setValue('-0x80');
      expect(control.valid).toBeTruthy();
    });

  });

});
