(function () {

    "use strict";
    
    angular.module('app', [])
        .controller("AppController", AppController);
    
    function AppController ($http) { 
        var ctrl = this;

        ctrl.playlist = []; 

        ctrl.status = { 
            song: {
                title: "Name",
                artist: "Other Name"
            },
            progress: 30
        };

        activate();

        function activate () {
            updatePlaylist();
        }

        function updatePlaylist () { 
            $http.get('api/playlist').then(function (res) { 
                ctrl.playlist = res.data;
            });
        }
    }

})();
