var gulp = require('gulp'),
    shell = require('gulp-shell');

gulp.task('build-tests', shell.task([
  'elm-make --warn test/TestRunner.elm --output test.js'
]))

gulp.task('run-tests', ['build-tests'], shell.task([
  'node test.js'
]))

gulp.task('default', function () {
  gulp.run('run-tests')

  gulp.watch(['src/**/*.elm', 'test/**/*.elm'], ['run-tests'])
})
