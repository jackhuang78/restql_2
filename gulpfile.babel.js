require('babel-core/register');
require('babel-polyfill');
import gulp from 'gulp';
import mocha from 'gulp-mocha';
import eslint from 'gulp-eslint';
import babel from 'gulp-babel';
import clean from 'gulp-clean';
import jsdoc from 'gulp-jsdoc3';
import {argv} from 'yargs';
import LoggerFactory from './src/LoggerFactory';

gulp.task('clean', () => {
	return gulp.src(['build', 'doc'])
 		.pipe(clean());
});

gulp.task('lint', () => {
	return gulp.src(['src/**/*.js', 'spec/**/*.js', './*.js'])
		.pipe(eslint())
		.pipe(eslint.format())
		.pipe(eslint.failAfterError());
});

gulp.task('build', ['clean'], () => {
	return gulp.src('src/**/*.js')
		.pipe(babel())
		.pipe(gulp.dest('build'));
});

gulp.task('doc', ['clean'], () => {
	return gulp.src('src/**/*.js')
		.pipe(jsdoc({
			plugins: ['node_modules/jsdoc-strip-async-await'], 
			opts: {destination: 'doc'}
		}));
});

gulp.task('test', ['lint'], () => {
	let file = (argv.f != null) ? argv.f : '**/*';
	if(argv.d != null)
		LoggerFactory.setLevel(this, 'debug');
	return gulp.src(`spec/${file}.spec.js`)
		.pipe(mocha({grep: argv.grep}));
});

gulp.task('default', ['test'], () => {
	console.log('Hello World!');
});