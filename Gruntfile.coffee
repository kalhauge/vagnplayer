module.exports = (grunt) ->
  grunt.initConfig
    watch:
      coffee:
        files: ['www/scripts/**/*.coffee']
        tasks: ['coffe:all']
      
    coffee:
      all:
        expand: true
        src: ['**/*.coffee']
        cwd: 'www/script'
        combine: true,
        out: 'public/app.js'
    
    grunt.loadNpmTasks 'grunt-contrib-watch'
    grunt.loadNpmTasks 'grunt-contrib-coffee'
    grunt.registerTask 'default', ['coffee:all']
