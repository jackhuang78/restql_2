require('babel-core/register');
import gulp from 'gulp';
import mocha from 'gulp-mocha';
import eslint from 'gulp-eslint';
import {argv} from 'yargs';
import LoggerFactory from './src/LoggerFactory';

gulp.task('lint', () => {
	return gulp.src(['src/**/*.js', 'spec/**/*.js', './*.js'])
		.pipe(eslint())
		.pipe(eslint.format())
		.pipe(eslint.failAfterError());
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