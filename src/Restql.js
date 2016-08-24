import {expect} from 'chai';
import {Enum} from 'enumify';

class CondOp extends Enum {}
CondOp.initEnum({
	EQ: { sym: '==' },
	NE: { sym: '!=' },
	GE: { sym: '>=' },
	GT: { sym: '>'  },
	LE: { sym: '<=' },
	LT: { sym: '<'  }
});
CondOp.parse = (sym) => {
	switch(sym.substring(0,2)) {
		case CondOp.EQ.sym: return [CondOp.EQ, sym.substring(2)];
		case CondOp.NE.sym: return [CondOp.NE, sym.substring(2)];
		case CondOp.GE.sym: return [CondOp.GE, sym.substring(2)];
		case CondOp.LE.sym: return [CondOp.LE, sym.substring(2)];
	}
	switch(sym.substring(0,1)) {
		case CondOp.GT.sym: return [CondOp.GT, sym.substring(1)];
		case CondOp.LT.sym: return [CondOp.LT, sym.substring(1)];
	}
	return [CondOp.EQ, sym];
};

class Restql {
	constructor() {
		
	}

	parseQueryParams(queryParams) {
		let obj = {};

		for(let field in queryParams) {
			let value = queryParams[field];
			let condOp = null;

			[condOp, value] = CondOp.parse(value);


			switch(value) {
				case '':
				case 'undefined':
				 obj[field] = [condOp, undefined];
				 break;

				case 'null':
					obj[field] = [condOp, null];
					break;

				default:
					if(value.length >= 2 && value[0] == '"' && value[value.length - 1] == '"')
						value = value.slice(1, -1);
					obj[field] = [condOp, value];
			}
			
		}

		return obj;
	}
}

export {Restql as default, CondOp};