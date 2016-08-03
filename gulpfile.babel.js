require('babel-core/register');
import gulp from 'gulp';
import mocha from 'gulp-mocha';
import eslint from 'gulp-eslint';
import {argv} from 'yargs';


gulp.task('default', () => {
	console.log('Hello World!');
});

gulp.task('lint', () => {
	return gulp.src(['src/**/*.js', 'spec/**/*.js', './*.js'])
		.pipe(eslint())
		.pipe(eslint.format())
		.pipe(eslint.failAfterError());
});

gulp.task('test', ['lint'], () => {
	let file = (argv.f != null) ? argv.f : '**/*';
	return gulp.src(`spec/${file}.js`)
		.pipe(mocha({grep: argv.grep}));
});