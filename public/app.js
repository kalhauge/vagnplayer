(function () {
    "use strict";
    
    angular.module('app', [])
        .controller("AppController", AppController)
        .directive("songSearcher", songSearcher);

    function songSearcher() {
        return {
            restrict: "A",
            link: link
        };
        function link (scope, elem) {
            var songs = new Bloodhound({
                datumTokenizer : Bloodhound.tokenizers.whitespace,
                queryTokenizer : Bloodhound.tokenizers.whitespace,
                identify : function (d) { return d.path;},
                remote: {
                    url : 'api/song?title=%QUERY',
                    wildcard: '%QUERY'    
                }
            });
            elem.typeahead({
                hint: true,
                highlight: true,
                minLength: 1
            }, { 
                display: Handlebars.compile('{{{title}}} - {{{artist}}}'),
                templates: {
                    suggestion: Handlebars.compile('<div><strong>{{title}}</strong> – {{artist}}</div>')
                },
                name : "songs", 
                source : songs 
            });
            elem.on('typeahead:select', function (event, song) {
                console.log(song);
            });
        }
    }
    
    function AppController ($http, $interval) { 
        var ctrl = this;

        ctrl.playlist = []; 

        ctrl.serverStatus = {
            state : 'PAUSED'
        };
        ctrl.status = null;

        ctrl.play  = control("PLAY");
        ctrl.pause = control("PAUSE");
        ctrl.prev  = control("PREV");
        ctrl.next  = control("NEXT");

        ctrl.isPaused = isState('PAUSED');
        ctrl.isPlaying = isState('PLAYING');

        activate();

        function activate () {
            updatePlaylist();
            updateStatus();
            $interval(reset, 3000);
        }

        function updatePlaylist () { 
            $http.get('api/playlist').then(function (res) { 
                ctrl.playlist = res.data;
            });
        }
        
        function updateStatus () {
            $http.get('api/status').then(function (res) {
                ctrl.serverStatus = res.data;
                ctrl.serverStatus.recievedAt = (new Date()).getTime();
                reset();
            });
        }

        function isState(en) {
            return function () {
                return ctrl.status.state == en;
            };
        }

        function interpolate(status) {
            var now = (new Date()).getTime();
            var secondsSince = ((now - status.recievedAt) / 1000);
            var time = status.time + (status.state == 'PLAYING' ? secondsSince : 0);
            var progress = 100 * (time / status.song.length);
            return {
                state : status.state,
                song : status.song,
                time : time, 
                progress : progress
            };
        }

        function reset() {
            ctrl.status = interpolate(ctrl.serverStatus);
        }
        
        function control (msg) {
            return function () { 
                $http.put('api/control', { cmd: msg }).then(updateStatus);
            };
        }
    }
})();
