import chai,{expect} from 'chai';
import Restql from '../src/Restql';

describe('# Restql', () => {
	describe('# constructor', () => {
		it('should create Restql instance', () => {
			expect(new Restql()).to.be.not.null;
		});
	});
});