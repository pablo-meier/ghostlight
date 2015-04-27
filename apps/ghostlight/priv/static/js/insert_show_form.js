var GHOSTLIGHT_EDIT =

(function(){
  'use strict';

var updateMode = false;
var workId;

var authors = {
    create: makeAuthorRow,
    gather: gatherAuthors,
    _currId: 0,
    _objCreators: []
};

function makeAuthorRow(author) {

  var authorTypePerson = $('<input type="radio" name="authorType' + authors._currId + '"/><label for="">Person</label>');
  var authorTypeOrg = $('<input type="radio" name="authorType' + authors._currId + '"/><label for="">Organization</label>');
  if (author && _.has(author, 'org')) {
    authorTypeOrg.attr('checked', 'checked'); 
  } else {
    authorTypePerson.attr('checked', 'checked'); 
  }

  var typeLabelDOM = $('<label>Type:</label>');
  var linkTypeWrapper = $('<div class="small-3 columns" />').append(typeLabelDOM).append(authorTypePerson).append(authorTypeOrg);

  var nameField = $('<input />', { 'type': 'text', 'placeholder': getNamePlaceholder() /*, name: 'linkLink' + externalLinks._currId */ });

  if (author !== undefined) {
    if (_.has(author, 'person')) {
      nameField.val(author.person.name);
    } else {
      nameField.val(author.org.name);
    }
  }

  var labeled = $('<label>Name:</label>').append(nameField);
  var linkTextWrapper = $('<div class="small-7 columns" />').append(labeled);

  var removeButton = $('<div class="small-2 columns"><div class="button round alert center less-rows-button small">Remove</div></div>');
  var rowToAdd = $('<div class="row" />').append(linkTypeWrapper).append(linkTextWrapper).append(removeButton);

  var index = authors._currId;

  authors._objCreators.push(
          {'id': index,
           'valueFunction' : function() {
                               var fieldType = authorTypeOrg.is(':checked') ? 'org' : 'person';
                               var returnObj = {};
                               var personOrOrg = { 'name' : nameField.val() };
                               returnObj[fieldType] = personOrOrg;
                               return returnObj;
                             }
          });

  removeButton.on('click', function() {
    rowToAdd.remove();
    var newCreators = _.filter(authors._objCreators, function(fnPair){
       return index !== fnPair.id; 
    });
    authors._objCreators = newCreators;
  });

  $('#linkArray').append(rowToAdd);
  authors._currId++;
}

function gatherAuthors() {
  return _.map(authors._objCreators, function(fnPair) {
    return fnPair.valueFunction();
  });
}


/////////////////////////////////////////////////////////
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

  var title = document['work-form']['work-title'].value;
  var desc = document['work-form']['work-description'].value;
  var collabOrg = document['work-form']['collab-org'].value;

  var finalObject = {
    'id': workId,
    'title': title,
    'description': desc,
    'authors': authors.gather()
  };

  if (collabOrg !== '' && collabOrg !== undefined) {
    finalObject.collaborating_org = { 'name' : collabOrg};
  }

  if (updateMode) finalObject.id = workId;

  var options = {
    'data': JSON.stringify(finalObject),
    'contentType': 'application/json',
    'dataType': 'json'
  };

  if (updateMode) {
    options.type= 'PUT';
    options.url = '/works/' + workId;
  } else {
    options.type = 'POST';
    options.url = '/works/';
  }

  $.ajax(options)
      .done(function() {
        var closeButton = $('<button href="#" tabindex="0" class="close" aria-label="Close Alert">&times;</button>');
        var alertBox = $('<div id="mainAlert1" data-alert class="alert-box success radius" tabindex="0" aria-live="assertive" role="dialogalert">Work submitted! Yay!</div>');
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
$('#addAuthorButton').on('click', noArgThunkify(authors.create));

function setStartData(workObj) {
  workId = workObj.id;
  document['work-form']['work-title'].value = workObj.title;

  if (_.has(workObj, 'collaborating_org')) {
    document['work-form']['collab-org'].value = workObj.collaborating_org;
  }
  if (_.has(workObj, 'description')) {
    document['work-form']['work-description'].value = workObj.description;
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
