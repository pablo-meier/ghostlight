var GHOSTLIGHT_EDIT =

(function(){
  'use strict';

// TODO
//   Validation?
//   Abstraction?

var updateMode = false;
var orgId;

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

function getLinkPlaceholder() {
  var linkPlaceHolders = [
    'http://zombo.com',
    'https://www.youtube.com/watch?v=ygI-2F8ApUM',
    'https://twitter.com/dril'
  ];
 
  return randomElementFrom(linkPlaceHolders);
}


/////////////////////////////////////////////////////////////

var employees = {
  'create': makeEmployeeRow,
  'gather': gatherEmployees,
  '_currId': 0,
  '_objCreators': []
};

function makeEmployeeRow(employee) {
  var titleInput = $('<input />', {'type': 'text', 'placeholder': getTitlePlaceholder() });
  var nameInput = $('<input />', {'type': 'text', 'placeholder': getNamePlaceholder() });
  var descriptionInput = $('<textarea />', {'placeholder': getDescriptionPlaceholder() });

  var titleLabel = $('<label>Title:</label>').append(titleInput);
  var nameLabel = $('<label>Name:</label>').append(nameInput);
  var descLabel = $('<label>Description:</label>').append(descriptionInput);

  var detailsCol = $('<div class="small-10 columns" />').append(titleLabel).append(nameLabel).append(descLabel);

  if (employee !== undefined) {
    valueIfHas(employee, 'person.name', nameInput);
    valueIfHas(employee, 'title', titleInput);
    valueIfHas(employee, 'description', descriptionInput);
  }

  var removeButton = $('<div class="button round alert center less-rows-button small">Remove</div>');
  var removeCol = $('<div class="small-2 columns">').append(removeButton);

  var rowToAdd = $('<div class="row" />').append(detailsCol).append(removeCol);

  var index = employees._currId;
  removeButton.on('click', function() {
    rowToAdd.remove();
    var newCreators = _.filter(employees._objCreators, function(fnPair) {
       return index !== fnPair.id; 
    });
    employees._objCreators = newCreators;
  });

  employees._objCreators.push({
    'id': employees._currId,
    'valueFunction': function() {
      return {
        'person' : {'name' : nameInput.val() },
        'title' : titleInput.val(),
        'description': descriptionInput.val()
      };
    }
  });

  $('#employeeArray').append(rowToAdd);
  employees._currId++;
}

function gatherEmployees() {
  return _.map(employees._objCreators, function(fnPair) {
    return fnPair.valueFunction();
  });
}


/////////////////////////////////////////////////////////////

var members = {
  'create': makeMemberRow,
  'gather': gatherMembers,
  '_currId': 0,
  '_objCreators': []
};


function makeMemberRow(member) {
  var nameInput = $('<input />', {'type': 'text', 'placeholder': getNamePlaceholder() });
  var descriptionInput = $('<textarea />', {'placeholder': getDescriptionPlaceholder() });

  if (member !== undefined) {
    valueIfHas(member, 'person.name', nameInput);
    valueIfHas(member, 'description', descriptionInput);
  }

  var nameLabel = $('<label>Name:</label>').append(nameInput);
  var descLabel = $('<label>Description:</label>').append(descriptionInput);

  var detailsCol = $('<div class="small-10 columns" />').append(nameLabel).append(descLabel);

  var removeButton = $('<div class="button round alert center less-rows-button small">Remove</div>');
  var removeCol = $('<div class="small-2 columns">').append(removeButton);

  var rowToAdd = $('<div class="row" />').append(detailsCol).append(removeCol);

  var index = members._currId;
  removeButton.on('click', function() {
    rowToAdd.remove();
    var newCreators = _.filter(members._objCreators, function(fnPair) {
       return index !== fnPair.id; 
    });
    members._objCreators = newCreators;
  });

  members._objCreators.push({
    'id': members._currId,
    'valueFunction': function() {
      return {
        'person' : {'name' : nameInput.val() },
        'description': descriptionInput.val()
      };
    }
  });

  $('#memberArray').append(rowToAdd);
  members._currId++;
}

function gatherMembers() {
  return _.map(members._objCreators, function(fnPair) {
    return fnPair.valueFunction();
  });
}

/////////////////////////////////////////////////////////
function valueIfHas(obj, property, domElement) {
  if (_.has(obj, property)) {
    domElement.val(_.get(obj, property));
  }
}

function getTitlePlaceholder() {
  var titlePlaceHolders = [
    'Chief Flossing Officer',
    'BACKUP KING'
  ];
 
  return randomElementFrom(titlePlaceHolders);
}

function getNamePlaceholder() {
  var namePlaceHolders = [
    'Bilbo McSwaggins',
    'Marty McCloud'
  ];
 
  return randomElementFrom(namePlaceHolders);
}

function getDescriptionPlaceholder() {
  var descriptionPlaceHolders = [
    'ya, last name spelled: [distant howl] as in far off train, [metallic scraping] as in axe being dragged through quarry, [quiet sobbing] as in',
    '((restrained by cops and forced to watch a man put mustard on a bagel) nno!! you\'re ruining it! That\'s quality bakedgoods'
  ];
 
  return randomElementFrom(descriptionPlaceHolders);
}


function randomElementFrom(arr) {
  return arr[Math.floor(Math.random() * arr.length)];
}


/**
 * Goes through the form, collects the values, constructs the appropriate object,
 * and posts a request. Update page for success/failure.
 */
function submitForm() {

  var name = document['org-form']['org-name'].value;
  var desc = document['org-form']['org-description'].value;
  var tagline = document['org-form']['org-tagline'].value;

  var finalObject = {
    'name': name,
    'tagline': tagline,
    'description': desc,
    'social': externalLinks.gather(),
    'employees': employees.gather(),
    'members': members.gather()
  };

  var options;
  if (updateMode) {
    finalObject.id = orgId;
    options = {
      'type': 'PUT',
      'url': '/organizations/' + orgId,
      'data': JSON.stringify(finalObject),
      'contentType': 'application/json',
      'dataType': 'json'
    };
  } else {
    options = {
      'type': 'POST',
      'url': '/organizations',
      'data': JSON.stringify(finalObject),
      'contentType': 'application/json',
      'dataType': 'json'
    };
  }

  $.ajax(options)
      .done(function() {
        var closeButton = $('<button href="#" tabindex="0" class="close" aria-label="Close Alert">&times;</button>');
        var alertBox = $('<div id="mainAlert1" data-alert class="alert-box success radius" tabindex="0" aria-live="assertive" role="dialogalert">Organization submitted! Yay!</div>');
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


$('#submitButton').on('click', submitForm);
$('#addLinkButton').on('click', externalLinks.create);
$('#addEmployeeButton').on('click', employees.create);
$('#addMemberButton').on('click', members.create);

function setStartData(orgObj) {
  orgId = orgObj.id;
  document['org-form']['org-name'].value = orgObj.name;
  if (_.has(orgObj, 'tagline')) {
    document['org-form']['org-tagline'].value = orgObj.tagline;
  }
  if (_.has(orgObj, 'description')) {
    document['org-form']['org-description'].value = orgObj.description;
  }

  if (_.has(orgObj, 'social')) {
    _.pairs(orgObj.social).forEach(function(socialPair) {
      console.log('socialPair is', socialPair);
      externalLinks.create(socialPair[0], socialPair[1]);
    });
  }

  if (_.has(orgObj, 'employees')) {
    orgObj.employees.forEach(function(emp) {
      employees.create(emp);
    });
  }

  if (_.has(orgObj, 'members')) {
    orgObj.members.forEach(function(mem) {
      members.create(mem);
    });
  }

  updateMode = true;
}

return setStartData;

})();
