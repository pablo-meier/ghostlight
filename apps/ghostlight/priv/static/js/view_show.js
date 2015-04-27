var GHOSTLIGHT_VIEW =

(function() {
  return function(dates) {
    var data = {};
    dates.forEach(function(d) { data[ (d.getTime() / 1000).toString() ] = 10; });

    var first = dates[0];
    var last = dates[dates.length - 1];
    var monthsApart = moment.range(first, last).diff('months') + 1;

    var options = {
      itemSelector: '#dateview',
      domain: 'month',
      subDomain: 'x_day',
      domainLabelFormat: '%B %Y',
      subDomainTitleFormat: {
        empty: '{date}',
        filled: '{date}'
      },
      tooltip: true,
      cellSize: 15,
      weekStartOnMonday: false,
      domainGutter: 8,
      displayLegend: false,
      range: monthsApart,
      legend: [10],
      start: dates[0],
      dataType: 'json',
      data: data
    };
    var cal = new CalHeatMap();
    cal.init(options);
  };
})();
