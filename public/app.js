(function () {

    "use strict";
    
    angular.module('app', [])
        .controller("AppController", AppController);
    
    function AppController ($http, $interval) { 
        var ctrl = this;

        ctrl.playlist = []; 

        ctrl.serverStatus = {
            state : 'PAUSED'
        };
        ctrl.status = {
            state : 'PAUSED'
        };

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
            $interval(function () { ctrl.status = interpolate(ctrl.serverStatus); }, 3000);
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
            var time = status.time + secondsSince;
            var progress = 100 * (time / status.song.length);
            return {
                state : status.state,
                song : status.song,
                time : time, 
                progress : progress
            };
        }
        
        function control (msg) {
            return function () { 
                $http.put('api/control', { cmd: msg });
            };
        }
    }

})();
