var gulp = require('gulp');
var sass = require('gulp-sass');
var concat = require('gulp-concat');
var sourcemaps = require('gulp-sourcemaps');
var source = require('vinyl-source-stream');
var buffer = require('vinyl-buffer');
var browserify = require('browserify');
var babel = require('babelify');


var mainScriptPath = './src/js/app.js'
var distFolder = "./dist"
var publicFolder = distFolder + "/public"

gulp.task('copy-fonts', function() {
  gulp
    .src('src/fonts/*')
    .pipe(gulp.dest(publicFolder+'/fonts/'));
})

gulp.task('copy-images', function() {
  gulp
    .src('src/images/*')
    .pipe(gulp.dest(publicFolder+'/images/'));
})

gulp.task('copy-html', function() {
  gulp
    .src('src/index.html')
    .pipe(gulp.dest(distFolder));
})

gulp.task('scripts', function() {
  return browserify(mainScriptPath, { debug: true, paths: ['./node_modules','./src/'] })
    .transform("babelify", {presets: ["es2015", "react", "stage-0"], plugins: ["transform-decorators-legacy"]})
    .bundle()
    .on('error', function(err) { console.error(err); this.emit('end'); })
    .pipe(source('app.js'))
    .pipe(buffer())
    .pipe(sourcemaps.init({ loadMaps: true }))
    .pipe(sourcemaps.write('./'))
    .pipe(gulp.dest(publicFolder+'/js'));
});


gulp.task('styles', function () {
  return gulp.src('./src/css/**/*.scss')
    .pipe(sass().on('error', sass.logError))
    .pipe(concat('app.css'))
    .pipe(gulp.dest(publicFolder+'/css'));
});

gulp.task('default', ['copy-html', 'copy-images', 'copy-fonts', 'scripts', 'styles'])
