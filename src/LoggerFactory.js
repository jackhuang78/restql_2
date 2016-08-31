import winston from 'winston';

class LoggerFactory {
	constructor() {
		let consoleTransport = new (winston.transports.Console)({colorize: true});
		let fileTransport = new (winston.transports.File)({filename: 'restql.log'});


		this.loggers = {
			default: new winston.Logger({
				level: 'info',
				transports: [consoleTransport, fileTransport]
			})
		};

		
	}

	classOf(obj) {
		return (obj == null || typeof(obj) != 'object' || !(obj.constructor.name in this.loggers))
			? 'default'
			: obj.constructor.name;
		
	}

	setLevel(obj, level) {
		let logger = this.getLogger(obj);
		for(let trans in logger.transports) {
			logger.transports[trans].level = level;
		}
	}

	getLogger(obj) {
		return this.loggers[this.classOf(obj)];
	}
}

export default new LoggerFactory();