(function () {
    "use strict";
    
    angular.module('app', [])
        .controller("AppController", AppController)
        .directive("songSearcher", songSearcher);

    function songSearcher($http) {
        return {
            restrict: "A",
            link: link
        };
        function link (scope, elem, ctrl, x) {
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
                    suggestion: Handlebars.compile(
                        '<div><strong>{{title}}</strong> – {{artist}}</div>'
                    )
                },
                name : "songs", 
                source : songs 
            });
            
            elem.on('typeahead:select', function (event, song) {
                if (song) {
                    scope.ctrl.addSong(song).then(function () {
                        elem.val('');
                    });
                }
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

        ctrl.addSong = addSong;

        activate();

        function activate () {
            updatePlaylist();
            updateStatus();
            $interval(reset, 3000);
        }

        function updatePlaylist () { 
            $http.get('api/playlist').then(function (res) { 
                ctrl.serverPlaylist = res.data;
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
            var song = status.song;
            while (time >= song.length) {
                time -= song.length;
                song = ctrl.serverPlaylist[song.index + 1];
            }
            var progress = 100 * (time / status.song.length);
            return {
                state : status.state,
                song : song,
                time : time, 
                progress : progress
            };
        }

        function reset() {
            ctrl.status = interpolate(ctrl.serverStatus);
            if (ctrl.status.song.index !== null) { 
                ctrl.playlist = ctrl.serverPlaylist.slice(ctrl.status.song.index + 1);
            }
        }
        
        function control (msg) {
            return function () { 
                $http.put('api/control', { cmd: msg }).then(updateStatus);
            };
        }

        function addSong (song) {
            return $http.put('api/playlist/' + song.path)
                .then(updatePlaylist);
        }
    }
})();
