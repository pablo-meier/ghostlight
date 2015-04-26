var GHOSTLIGHT_EDIT =

(function(){
  'use strict';

var updateMode = false;
var personId;

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
function getLinkPlaceholder() {
  var linkPlaceHolders = [
    'http://zombo.com',
    'https://www.youtube.com/watch?v=ygI-2F8ApUM',
    'https://twitter.com/dril'
  ];
 
  return randomElementFrom(linkPlaceHolders);
}

function randomElementFrom(arr) {
  return arr[Math.floor(Math.random() * arr.length)];
}


/**
 * Goes through the form, collects the values, constructs the appropriate object,
 * and posts a request. Update page for success/failure.
 */
function submitForm() {

  var name = document['person-form']['person-name'].value;
  var desc = document['person-form']['person-description'].value;

  var finalObject = {
    'name': name,
    'description': desc,
    'social': externalLinks.gather()
  };

  var options;
  if (updateMode) {
    finalObject.id = personId;
    options = {
      'type': 'PUT',
      'url': '/people/' + personId,
      'data': JSON.stringify(finalObject),
      'contentType': 'application/json',
      'dataType': 'json'
    };
  } else {
    options = {
      'type': 'POST',
      'url': '/people/',
      'data': JSON.stringify(finalObject),
      'contentType': 'application/json',
      'dataType': 'json'
    };
  }

  $.ajax(options)
      .done(function() {
        var closeButton = $('<button href="#" tabindex="0" class="close" aria-label="Close Alert">&times;</button>');
        var alertBox = $('<div id="mainAlert1" data-alert class="alert-box success radius" tabindex="0" aria-live="assertive" role="dialogalert">Person submitted! Yay!</div>');
        alertBox.append(closeButton);
        closeButton.on('click', function() {
          jQuery.fadeOut(500, function() {
            alertBox.remove();
          });
        });
        $('body').append(alertBox);
      })
      .fail(function() {
        var closeButton = $('<button href="#" tabindex="0" class="close" aria-label="Close Alert">&times;</button>');
        var alertBox = $('<div id="mainAlert1" data-alert class="alert-box success radius" tabindex="0" aria-live="assertive" role="dialogalert">Ruh-roh! Something went wrong.</div>');
        alertBox.append(closeButton);
        closeButton.on('click', function() {
          jQuery.fadeOut(500, function() {
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

function setStartData(personObj) {
  personId = personObj.id;
  document['person-form']['person-name'].value = personObj.name;
  if (_.has(personObj, 'description')) {
    document['person-form']['person-description'].value = personObj.description;
  }

  if (_.has(personObj, 'social')) {
    _.pairs(personObj.social).forEach(function(socialPair) {
      externalLinks.create(socialPair[0], socialPair[1]);
    });
  }

  updateMode = true;
}

return setStartData;

})();
