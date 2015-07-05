"use strict";
var _ = require('lodash');
var async = require('async');
var fs = require('fs');
var path = require('path');
var request = require('request');
var util = require('util');

var resources = ['organizations', 'shows', 'works', 'people'];
//var url = 'http://127.0.0.1:8080/%s';
var url = 'https://ghostlight.io/%s';

resources.forEach(function(resource) {
  var resourcePath = path.join('testdata', resource);
  var contents = fs.readdirSync(resourcePath);

  async.eachSeries(contents, function(entity, cb) {
    var entityPath = path.join(resourcePath, entity);
    if (entityPath.match(/^\./)) return;
    var contents = fs.readFileSync(entityPath);
    var options = {
      uri: util.format(url, resource),
      auth: {
        user: 'casper',
        pass: 'fantasma_porfavor'
      },
      method: "POST",
      headers: {
        'content-type' : 'application/json'
      },
      body: contents
    };

    console.log('Firing off for ', entity);
    request(options, function(err, msg, body) {
      if (err) {
        console.error('  ', entity, 'FAILURE:', err);
        return;
      }
      console.log('   ', entity, 'Success!', body);
      setTimeout(function() {
        cb();
      }, 400);
    });
  });
});


