import chai,{expect} from 'chai';
import Restql,{CondOp} from '../src/Restql';

describe('# Restql', () => {
	describe('# constructor', () => {
		it('should create empty Restql instance', () => {
			expect(new Restql()).to.be.not.null;
		});
	});

	describe('# parseQueryParams', () => {
		let restql = null;
		beforeEach(() => {
			restql = new Restql();
		});

		it('should parse basic query', () => {
			expect(restql.parseQueryParams({
				f1:'1', 
				f2:'', 
				f3:'null', 
				f4:'undefined',
			})).to.deep.equal({
				f1:[CondOp.EQ,'1'], 
				f2:[CondOp.EQ,undefined], 
				f3:[CondOp.EQ,null], 
				f4:[CondOp.EQ,undefined],
			});
		});

		it('should parse query with quotes', () => {
			expect(restql.parseQueryParams({
				f1:'"1"', 
				f2:'""', 
				f3:'"null"', 
				f4:'"undefined"',
			})).to.deep.equal({
				f1:[CondOp.EQ,'1'], 
				f2:[CondOp.EQ,''], 
				f3:[CondOp.EQ,'null'], 
				f4:[CondOp.EQ,'undefined'],
			});
		});

		it('should parse query with conditional operator', () => {
			expect(restql.parseQueryParams({
				f1:'=1', 
				f2:'!=2', 
				f3:'>3', 
				f4:'>=4', 
				f5:'<5', 
				f6:'<=6',
				f7:'~7',
				f8:'!~8',
			})).to.deep.equal({
				f1:[CondOp.EQ,'1'], 
				f2:[CondOp.NE,'2'], 
				f3:[CondOp.GT,'3'], 
				f4:[CondOp.GE,'4'],
				f5:[CondOp.LT,'5'], 
				f6:[CondOp.LE,'6'],
				f7:[CondOp.LK,'7'],
				f8:[CondOp.NL,'8'],
			});
		});
	});
});