import util from 'util';
import {expect} from 'chai';
import {Enum} from 'enumify';
import LoggerFactory from './LoggerFactory.js';

class CondOp extends Enum {}
CondOp.initEnum({
	EQ: { sym: '='  },
	NE: { sym: '!=' },
	GE: { sym: '>=' },
	GT: { sym: '>'  },
	LE: { sym: '<=' },
	LT: { sym: '<'  },
	LK: { sym: '~'  },
	NL: { sym: '!~' },
});
CondOp.parse = (sym) => {
	switch(sym.substring(0,2)) {
		case CondOp.NE.sym: return [CondOp.NE, sym.substring(2)];
		case CondOp.GE.sym: return [CondOp.GE, sym.substring(2)];
		case CondOp.LE.sym: return [CondOp.LE, sym.substring(2)];
		case CondOp.NL.sym: return [CondOp.NL, sym.substring(2)];
	}
	switch(sym.substring(0,1)) {
		case CondOp.EQ.sym: return [CondOp.EQ, sym.substring(1)];
		case CondOp.GT.sym: return [CondOp.GT, sym.substring(1)];
		case CondOp.LT.sym: return [CondOp.LT, sym.substring(1)];
		case CondOp.LK.sym: return [CondOp.LK, sym.substring(1)];
	}
	return [CondOp.EQ, sym];
};

class Restql {
	constructor() {
		this.logger = LoggerFactory.getLogger(this);
	}

	parseQueryParams(queryParams) {
		//this.logger.verbose(`parse ${util.inspect(queryParams)}`);	

		let obj = {};

		for(let field in queryParams) {
			//console.log(`parsing ${queryParams[field]}`);

			let [condOp, value] = CondOp.parse(queryParams[field].trim());

			let m = value.match(/({.*})/);
			if(m != null) {
				obj[field] = [condOp, this.parseQueryParams(m[1])];
				continue;
			}

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