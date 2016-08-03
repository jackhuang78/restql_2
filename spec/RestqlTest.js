require('babel-core/register');

import chai,{expect} from 'chai';
import Restql from '../src/Restql';

describe('# Restql', () => {
	describe('# constructor', () => {
		it('should create RestQL instance', () => {
			expect(new Restql()).to.be.not.null;
		});
	});
});