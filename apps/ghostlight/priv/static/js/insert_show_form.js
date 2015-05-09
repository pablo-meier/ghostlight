var GHOSTLIGHT_EDIT =

(function(){
  'use strict';

var updateMode = false;
var showId;

/*
 * Allows constructor use for a type that will be attached to an array in a form.
 * Handles much of the boilerplate. The options object has a few mandatory fields:
 *
 * * 'rowCreate': Returns an object with two properties:
 *      'elements': a list DOM element that are <div class="column">s adding up to 10.
 *                  We'll attach to a row later.
 *      'gatherFun': A function we can call without arguments to gather its values.
 * * 'addButtonSelector': Selector for the add button.
 * * 'arrayRowsSelector': Selector for where to add the rows.
 * * 'gatherType' : Either 'object' or 'array'. Tells us how to use the values
 *    we get back.
 *
 * Calling new on this with those options returns an object that has two methods:
 *
 * * 'create': Call this function to add a row to the form's array. Note that you
 *    if called with an argument, it get passed to rowCreate to set values in the
 *    fields (useful for editMode).
 * * 'gather': Call without arguments to get whatever this form requires (array of
 *    things, an object like the 'social' block, etc.).
 */
function makeArrayable(options) {
  var currId = 0;
  var objCreators = [];

  var rowCreate = options.rowCreate;
  var arrayRowsSelector = options.arrayRowsSelector;
  var addButtonSelector = options.addButtonSelector;
  var gatherType = options.gatherType;

  var createFunction = function() {
    var rowCreated = rowCreate.apply(this, arguments);
    var colsToAdd = rowCreated.elements;

    var removeButton = $('<div class="small-2 columns"><div class="button round alert center less-rows-button small">Remove</div></div>');
    var rowToAdd = $('<div class="row" />');
    colsToAdd.forEach(function(col) {
      rowToAdd.append(col);
    });
    rowToAdd.append(removeButton);

    var index = currId;
    objCreators.push({
      'id': index,
      'valueFunction' : rowCreated.gatherFun
    });
    removeButton.on('click', function() {
      rowToAdd.remove();
      var newCreators = _.filter(objCreators, function(fnPair){
         return index !== fnPair.id;
      });
      objCreators = newCreators;
    });

    $(arrayRowsSelector).append(rowToAdd);
    currId++;
  };

  var gatherFunction = function() {
    if (gatherType === 'object') {
      return _.reduce(objCreators, function(accum, fnPair) {
        var pair = fnPair.valueFunction();
        accum[pair[0]] = pair[1];
        return accum;
      }, {});
    } else if (gatherType === 'array') {
      return _.map(objCreators, function(fnPair) {
        return fnPair.valueFunction();
      });
    }
  };

  $(addButtonSelector).on('click', noArgThunkify(createFunction));

  return {
    'create': createFunction,
    'gather': gatherFunction,
  };
}

//////////////////////////////////////////

function makeProducerRow(producer) {

  var hackId = new Date().getTime();
  var producerTypePerson = $('<input type="radio" name="authorType' + hackId + '"/><label for="">Person</label>');
  var producerTypeOrg = $('<input type="radio" name="authorType' + hackId + '"/><label for="">Organization</label>');
  if (producer && _.has(producer, 'org')) {
    producerTypeOrg.attr('checked', 'checked'); 
  } else {
    producerTypePerson.attr('checked', 'checked'); 
  }
  var typeLabelDOM = $('<label>Type:</label>');
  var producerTypeWrapper = $('<div class="small-3 columns" />').append(typeLabelDOM).append(producerTypePerson).append(producerTypeOrg);

  var nameField = $('<input />', { 'type': 'text', 'placeholder': getNamePlaceholder() });

  if (producer !== undefined) {
    if (_.has(producer, 'person')) {
      nameField.val(producer.person.name);
    } else {
      nameField.val(producer.org.name);
    }
  }

  var labeled = $('<label>Name:</label>').append(nameField);
  var producerNameWrapper = $('<div class="small-7 columns" />').append(labeled);

  var elements = [producerTypeWrapper, producerNameWrapper];
  var gatherFun = function() {
                 var fieldType = producerTypeOrg.is(':checked') ? 'org' : 'person';
                 var returnObj = {};
                 var personOrOrg = { 'name' : nameField.val() };
                 returnObj[fieldType] = personOrOrg;
                 return returnObj;
  };
  return {
    'elements' : elements,
    'gatherFun' : gatherFun
  };
}


var producerOptions = {
  'rowCreate': makeProducerRow,
  'addButtonSelector': '#addProducerButton',
  'arrayRowsSelector': '#producerArray',
  'gatherType': 'array'
};
var producers = makeArrayable(producerOptions);


////////////////////////////////////////////// 

function makeLinkRow(linkType, linkUrl) {

  var linkPairs = [ ['website', 'Website'],
                    ['email', 'Email'],
                    ['blog', 'Blog'],
                    ['newsletter', 'Newsletter'],
                    ['facebook', 'Facebook'],
                    ['twitter', 'Twitter'],
                    ['instagram', 'Instagram'],
                    ['vimeo', 'Vimeo'],
                    ['youtube', 'YouTube'],
                    ['pinterest', 'Pinterest'],
                    ['tumblr', 'Tumblr'],
                    ['gplus', 'Google+ (lol)'],
                    ['patreon', 'Patreon'],
                    ['newplayx', 'NewPlay Exchange'] ];

  var linkSelectDOM = $('<select>');
  linkPairs.forEach(function(pair) {
    var option;
    if (linkType === pair[0]) {
      option = $('<option value="'+ pair[0] +'" selected="selected">'+ pair[1] +'</option>');
    } else {
      option = $('<option value="'+ pair[0] +'">'+ pair[1] +'</option>');
    }
    linkSelectDOM.append(option);
  });

  var withLabelDOM = $('<label>Type:</label>').append(linkSelectDOM);
  var linkTypeWrapper = $('<div class="small-2 columns" />').append(withLabelDOM);

  var linkField = $('<input />', { 'type': 'url', 'placeholder': getLinkPlaceholder(), name: 'linkLink' + externalLinks._currId });
  if (linkUrl !== undefined) {
    linkField.val(linkUrl);
  }
  var labeled = $('<label>Link:</label>').append(linkField);
  var linkTextWrapper = $('<div class="small-8 columns" />').append(labeled);

  var elements = [linkTypeWrapper, linkTextWrapper];
  var gatherFun = function() {
    return [linkSelectDOM.val(), linkField.val()];
  };
  return {
    'elements': elements,
    'gatherFun': gatherFun
  };
}



var externalLinkOptions = {
  'rowCreate': makeLinkRow,
  'addButtonSelector': '#addLinkButton',
  'arrayRowsSelector': '#linkArray',
  'gatherType': 'object'
};
var externalLinks = makeArrayable(externalLinkOptions);


/////////////////////////////////////////////////////////

function makePressLinkRow(linkDesc, linkUrl) {

  var descField = $('<input />', { 'type': 'url', 'placeholder': getPressLinkDescPlaceholder() });
  if (linkDesc !== undefined) {
    descField.val(linkDesc);
  }
  var descLabeled = $('<label>Press Description:</label>').append(descField);
  var linkDescWrapper = $('<div class="small-5 columns" />').append(descLabeled);

  var linkField = $('<input />', { 'type': 'url', 'placeholder': getLinkPlaceholder(), name: 'linkLink' + pressLinks._currId });
  if (linkUrl !== undefined) {
    linkField.val(linkUrl);
  }
  var labeled = $('<label>Link:</label>').append(linkField);
  var linkTextWrapper = $('<div class="small-5 columns" />').append(labeled);

  var elements = [linkDescWrapper, linkTextWrapper];
  var gatherFun = function() {
    return {
      'description': descField.val(),
      'link': linkField.val()
    };
  };

  return {
    'elements': elements,
    'gatherFun': gatherFun
  };
}

var pressLinkOptions = {
  'rowCreate': makePressLinkRow,
  'addButtonSelector': '#addPressButton',
  'arrayRowsSelector': '#pressArray',
  'gatherType': 'array'
};

var pressLinks = makeArrayable(pressLinkOptions);

/////////////////////////////////////////////////////////
function makeDateRow(date, time) {

  var dateField = $('<input />', { 'class': 'picker__input', 'placeholder': 'Date…' }).pickadate();
  if (date !== undefined) {
    dateField.val(date);
  }
  var dateLabeled = $('<label>Date:</label>').append(dateField);
  var dateWrapper = $('<div class="small-5 columns" />').append(dateLabeled);

  var timeField = $('<input />', { 'class': 'picker__input', 'placeholder': 'Time…' }).pickatime();
  if (time !== undefined) {
    timeField.val(time);
  }
  var timeLabeled = $('<label>Time:</label>').append(timeField);
  var timeWrapper = $('<div class="small-5 columns" />').append(timeLabeled);
 
  var elements = [dateWrapper, timeWrapper];
  var gatherFun = function() {
    return {
      'name': nameField.val()
    };
  };

  return {
    'elements': elements,
    'gatherFun': gatherFun
  };
}

var dateOptions = {
  'rowCreate': makeDateRow,
  'addButtonSelector': '#addDateButton',
  'arrayRowsSelector': '#datesArray',
  'gatherType': 'array'
};

var dates = makeArrayable(dateOptions);

/////////////////////////////////////////////////////////
function makePerformanceRow() {

  // One work
  //   with [1,several] person-or-org-authors
  //   an optional description if its new.
  //
  // [1,several] people as directors
  // Performance description
  // Director's Note
  // Onstage: [1,several] {role, person}
  // Offstage: [1,several] {job, person}
  //
  var workField = $('<input />', { 'type' : 'text', 'placeholder': '…' }).pickadate();
  if (date !== undefined) {
    dateField.val(date);
  }
  var dateLabeled = $('<label>Date:</label>').append(dateField);
  var dateWrapper = $('<div class="small-5 columns" />').append(dateLabeled);

  var timeField = $('<input />', { 'class': 'picker__input', 'placeholder': 'Time…' }).pickatime();
  if (time !== undefined) {
    timeField.val(time);
  }
  var timeLabeled = $('<label>Time:</label>').append(timeField);
  var timeWrapper = $('<div class="small-5 columns" />').append(timeLabeled);
 
  var elements = [dateWrapper, timeWrapper];
  var gatherFun = function() {
    return {
      'name': nameField.val()
    };
  };

  return {
    'elements': elements,
    'gatherFun': gatherFun
  };
}

var performanceOptions = {
  'rowCreate': makePerformanceRow,
  'addButtonSelector': '#addPerformanceButton',
  'arrayRowsSelector': '#performancesArray',
  'gatherType': 'array'
};

var performances = makeArrayable(performanceOptions);

/////////////////////////////////////////////////////////

function makeHostRow(name) {

  var nameField = $('<input />', { 'type': 'text', 'placeholder': getNamePlaceholder() });
  if (name !== undefined) {
    nameField.val(name);
  }
  var nameLabeled = $('<label>Host:</label>').append(nameField);
  var nameWrapper = $('<div class="small-10 columns" />').append(nameLabeled);

  var elements = [nameWrapper];
  var gatherFun = function() {
    return {
      'name': nameField.val()
    };
  };

  return {
    'elements': elements,
    'gatherFun': gatherFun
  };
}

var hostOptions = {
  'rowCreate': makeHostRow,
  'addButtonSelector': '#addHostButton',
  'arrayRowsSelector': '#hostsArray',
  'gatherType': 'array'
};

var hosts = makeArrayable(hostOptions);

/////////////////////////////////////////////////////////
function getLinkPlaceholder() {
  var linkPlaceHolders = [
    'http://zombo.com',
    'https://www.youtube.com/watch?v=ygI-2F8ApUM',
    'https://twitter.com/dril'
  ];

  return randomElementFrom(linkPlaceHolders);
}

function getPressLinkDescPlaceholder() {
  var pressDescPlaceholders = [
    'New York Post Expose',
    'Conservapedia Article'
  ];

  return randomElementFrom(pressDescPlaceholders);
}

function getNamePlaceholder() {
  var namePlaceHolders = [
    'Bilbo McSwaggins',
    'Marty McCloud'
  ];
 
  return randomElementFrom(namePlaceHolders);
}

function randomElementFrom(arr) {
  return arr[Math.floor(Math.random() * arr.length)];
}


/**
 * Goes through the form, collects the values, constructs the appropriate object,
 * and posts a request. Update page for success/failure.
 */
function submitForm() {

  var title = document['show-form']['show-title'].value;
  var desc = document['show-form']['show-description'].value;
  var specialThanks = document['show-form']['show-special-thanks'].value;

  var finalObject = {
    'title': title,
    'description': desc,
    'performances': performances.gather(),
    'social': externalLinks.gather(),
    'press': pressLinks.gather(),
    'hosts': hosts.gather(),
    'producers' : producers.gather(),
    'special_thanks': specialThanks
  };

  if (updateMode) finalObject.id = showId;

  var options = {
    'data': JSON.stringify(finalObject),
    'contentType': 'application/json',
    'dataType': 'json'
  };

  if (updateMode) {
    options.type= 'PUT';
    options.url = '/shows/' + showId;
  } else {
    options.type = 'POST';
    options.url = '/shows/';
  }

  $.ajax(options)
      .done(function() {
        var closeButton = $('<button href="#" tabindex="0" class="close" aria-label="Close Alert">&times;</button>');
        var alertBox = $('<div id="mainAlert1" data-alert class="alert-box success radius" tabindex="0" aria-live="assertive" role="dialogalert">Show submitted! Yay!</div>');
        alertBox.append(closeButton);
        closeButton.on('click', function() {
          alertBox.fadeOut(500, function() {
            alertBox.remove();
          });
        });
        $('body').append(alertBox);
      })
      .fail(function() {
        var closeButton = $('<button href="#" tabindex="0" class="close" aria-label="Close Alert">&times;</button>');
        var alertBox = $('<div id="mainAlert1" data-alert class="alert-box alert radius" tabindex="0" aria-live="assertive" role="dialogalert">Ruh-roh! Something went wrong.</div>');
        alertBox.append(closeButton);
        closeButton.on('click', function() {
          alertBox.fadeOut(500, function() {
            alertBox.remove();
          });
        });
        $('body').append(alertBox);
      });

  console.log('Submitting:', JSON.stringify(finalObject)); 
}

function noArgThunkify(fun) {
  return function() { fun(); };
}

$('#submitButton').on('click', submitForm);

function setStartData(showObj) {
  showId = showObj.id;
  document['show-form']['show-title'].value = showObj.title;

  if (_.has(workObj, 'description')) {
    document['show-form']['show-description'].value = showObj.description;
  }

  if (_.has(workObj, 'authors')) {
    workObj.authors.forEach(function(author) {
      authors.create(author);
    });
  }

  updateMode = true;
}

return setStartData;

})();
