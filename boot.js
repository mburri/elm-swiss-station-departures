window.onload = function () {
  var storageKey = "recent-searches";
  var storedData = localStorage.getItem(storageKey);
  var recentStations = storedData ? JSON.parse(storedData) : [];

  var app = Elm.Main.init({
    node: document.getElementById("elm"),
    flags: recentStations,
  });

  app.ports.setStorage.subscribe(function (recentStations) {
    localStorage.setItem(storageKey, JSON.stringify(recentStations));
  });
};
