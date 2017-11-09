class Connector {

	/**
	 * Create and initialize a connector. 
	 * A connector accepts basic CRUD operations and executes them 
	 * accordingly on the underlying data store.
	 * 
	 * @return {Connector} a connector to the underlying data store
	 */
	constructor() {

	}

	/**
	 * Returns a list of available collections in this data store.
	 * 
	 * @return {String[]} The list of available collections.
	 */
	collections() {

	}

	/**
	 * Returns a list of defined fields for a particular collection in the data store.
	 * 
	 * @param  {String}	collection A collection
	 * @return {String[]}	The list of available fields for the given collection.
	 */
	fields(collection) {

	}

	/**
	 * Create elements in the data store.
	 * 		
	 * @param  {String} collection 	The collection to create the elements under.
	 * @param  {Object[]} elements	The elements to create.
	 * @return {Object[]}	Object templates with ID for the created elements.
	 */
	create(collection, elements) {

	}

	/**
	 * Read elements from the data store.
	 *  
	 * @param  {String} collection	The collection to read elements from.
	 * @param  {Object} query	The query object. //TODO
	 * @return {Object[]}	Objects matching the query. 
	 */
	read(collection, query) {

	}

	/**
	 * [update description]
	 * @return {[type]} [description]
	 */
	update() {

	}

	delete() {

	}

	createRelation() {

	}

	readRelation() {

	}

	updateRelatieon() {

	}

	deleteRelation() {

	}

}

export default Connector;