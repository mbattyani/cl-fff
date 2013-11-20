var app = angular.module('app', []);
var routes = function ($routeProvider) {
    $routeProvider.when('/history-and-philosophy', { 'templateUrl' : '/history-and-philosophy.php', 'controller' : 'history-and-philosophy-controller' });
    $routeProvider.when('/who-we-are/management', { 'templateUrl' : '/management.html', 'controller' : 'management-controller' });
    return null;
};
app.config(routes);
function managementControllerFn($scope) {
    return $scope.variable = 'Maaanaaageemeeent';
};
app.controller('management-controller', managementControllerFn);
function historyAndPhilosophyControllerFn($scope, $location) {
    return $scope.name = 'Lispers';
};
app.controller('history-and-philosophy-controller', historyAndPhilosophyControllerFn);
