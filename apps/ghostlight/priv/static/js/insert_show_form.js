var GHOSTLIGHT_EDIT =

(function(){
  'use strict';

var updateMode = false;
var showId;

var externalLinks = {
    create: makeLinkRow,
    gather: gatherLinks,
    _currId: 0,
    _objCreators: []
};

/**
 * What happens after 'Add Link' is pressed.
 */
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

  var removeButton = $('<div class="small-2 columns"><div class="button round alert center less-rows-button small">Remove</div></div>');
  var rowToAdd = $('<div class="row" />').append(linkTypeWrapper).append(linkTextWrapper).append(removeButton);

  var index = externalLinks._currId;

  externalLinks._objCreators.push(
          {'id': index,
           'valueFunction' : function() {
                               return [linkSelectDOM.val(), linkField.val()];
                             }
          });

  removeButton.on('click', function() {
    rowToAdd.remove();
    var newCreators = _.filter(externalLinks._objCreators, function(fnPair){
       return index !== fnPair.id; 
    });
    externalLinks._objCreators = newCreators;
  });

  $('#linkArray').append(rowToAdd);
  externalLinks._currId++;
}

function gatherLinks() {
  return _.reduce(externalLinks._objCreators, function(accum, fnPair) {
    var pair = fnPair.valueFunction();
    accum[pair[0]] = pair[1];
    return accum;
  }, {});
}

/////////////////////////////////////////////////////////
var pressLinks = {
    create: makePressLinkRow,
    gather: gatherPress,
    _currId: 0,
    _objCreators: []
};

/**
 * What happens after 'Add Link' is pressed.
 */
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

  var removeButton = $('<div class="small-2 columns"><div class="button round alert center less-rows-button small">Remove</div></div>');
  var rowToAdd = $('<div class="row" />').append(linkDescWrapper).append(linkTextWrapper).append(removeButton);

  var index = pressLinks._currId;

  pressLinks._objCreators.push(
          {'id': index,
           'valueFunction' : function() {
                               return {
                                 'description': descField.val(),
                                 'link': linkField.val()
                               };
                             }
          });

  removeButton.on('click', function() {
    rowToAdd.remove();
    var newCreators = _.filter(pressLinks._objCreators, function(fnPair){
       return index !== fnPair.id; 
    });
    pressLinks._objCreators = newCreators;
  });

  $('#pressArray').append(rowToAdd);
  pressLinks._currId++;
}

function gatherPress() {
  return _.map(pressLinks._objCreators, function(fnPair) {
    return fnPair.valueFunction();
  });
}


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
    'social': externalLinks.gather(),
    'press': pressLinks.gather(),
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
$('#addLinkButton').on('click', noArgThunkify(externalLinks.create));
$('#addPressButton').on('click', noArgThunkify(pressLinks.create));

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
