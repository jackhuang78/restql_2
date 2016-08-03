import gulp from 'gulp';
import eslint from 'gulp-eslint';

gulp.task('default', () => {
	console.log('Hello World!')
})

gulp.task('lint', () => {
	return gulp.src(['src/**/*.js', 'test/**/*.js', './*.js'])
		.pipe(eslint())
		.pipe(eslint.format())
		.pipe(eslint.failAfterError());
});